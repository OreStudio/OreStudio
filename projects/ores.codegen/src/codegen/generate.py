import json
import logging
import shutil
import subprocess
from pathlib import Path
from typing import Any

from .core import (
    generate_from_model,
    get_model_type,
    load_model,
    resolve_output_path,
)
from .physical_space import (
    _enabled_overrides,
    address_supports_model_type,
    compute_supported_set,
    compute_target_set,
    is_enabled,
    kind_matches,
    load_graph,
    resolve_generation_set,
)

log = logging.getLogger(__name__)

# C++ source extensions that codegen runs clang-format over as a second step.
_CLANG_FORMAT_EXTS = {".cpp", ".hpp", ".h", ".cxx", ".cc"}


def clang_format_files(paths: list[Path]) -> None:
    """Run ``clang-format -i`` over the generated C++ files, twice.

    Codegen is a two-step process: render the template, then normalise the
    output with the project's ``.clang-format`` so template whitespace never
    shows up as spurious diffs. SQL/other artefacts are left untouched.
    No-op (with a warning) when clang-format is not installed.

    clang-format is not idempotent in a single pass for some multi-line
    trailing-comment patterns (e.g. a comment spanning several lines after
    ``field = // ...`` before a lambda): formatting raw, never-before-
    formatted text can land on an intermediate state that a second pass
    then reformats further, before settling into the true fixed point on
    the third pass onward. Running it twice here means codegen's own
    output always reaches the same fixed point that reformatting an
    already-formatted file converges to (verified in
    doc/agile/versions/v0/sprint_25/jwt_test_ttl_too_short/), so it can't
    drift from what a plain ``clang-format -i`` on checked-in files
    produces.
    """
    cpp = [p for p in paths if p.suffix in _CLANG_FORMAT_EXTS]
    if not cpp:
        return
    exe = shutil.which("clang-format")
    if exe is None:
        log.warning("clang-format not found on PATH; skipping format of %d "
                    "generated C++ file(s)", len(cpp))
        return
    str_paths = [str(p) for p in cpp]
    subprocess.run([exe, "-i", *str_paths], check=True)
    subprocess.run([exe, "-i", *str_paths], check=True)
    log.info("clang-formatted %d generated C++ file(s) (two passes)", len(cpp))

# Filter for org files in a component's modeling/ dir: only files whose
# frontmatter declares a codegen model type are picked up. Other org
# files (overviews, knowledge docs, plantuml source) are skipped.
from .manifest import is_codegen_entity_org as _is_codegen_entity_org  # noqa: E402


def _read_drawer_properties(model_path: Path) -> dict[str, Any]:
    """The model's file-level ``:PROPERTIES:`` drawer (org models only),
    merged with its effective ``* Physical space`` table overrides (its own
    table, plus its bound profile's table as defaults -- see
    ``read_physical_space_overrides``).

    This is where an entity's ``:ores.*.enabled:`` activation overrides live
    (as plain drawer properties, or via the table mechanism); JSON models
    carry no drawer, so they get an empty dict."""
    if model_path.suffix != ".org":
        return {}
    try:
        from .org_loader import parse_org, read_physical_space_overrides  # noqa: PLC0415
        doc = parse_org(model_path.read_text(encoding="utf-8"))
        properties = dict(doc.file_properties)
        properties.update(read_physical_space_overrides(doc))
        return properties
    except Exception:  # noqa: BLE001 — a malformed drawer must not break codegen
        return {}


# A junction's messaging layer exists to serve parent-scoped list reads
# (:list_by: on a junction side). With no :list_by: declared, the four
# facets would emit a stack nothing subscribes to: a registrar shell,
# an unreachable service, and a protocol nobody reads.
_JUNCTION_MESSAGING_FACETS = frozenset({
    "ores.cpp.nats-handler",
    "ores.cpp.nats-sub-registrar",
    "ores.cpp.protocol",
    "ores.cpp.service",
})


def resolve_targets(
    model_path: Path,
    base_dir: Path,
    *,
    address: str | None = None,
    properties: dict[str, Any] | None = None,
) -> tuple[list[dict], str, dict]:
    """Traverse the physical-space graph; return what to generate for a model.

    THE single resolver — used by the codegen CLI and the compass wrapper, so
    there is one place that maps (model, selector) → archetypes. ``address``
    restricts generation to a physical-space subtree; omitted, it generates
    the entity's full supported set.

    Returns ``(units, model_type, model_data)`` where each unit is
    ``{"template": <name>, "output": <project-root-relative path>}``.
    Raises ``ValueError`` on an unknown address.
    """
    graph = load_graph(base_dir / "library" / "templates")
    model_type = get_model_type(model_path.name, model_path)
    if properties is None:
        properties = _read_drawer_properties(model_path)

    target = compute_target_set(address, graph)

    # S_e: model-type-admissible facets narrowed by the entity's :ores.*.enabled:
    # drawer (read above from the model file; empty => full supported set).
    supported = compute_supported_set(properties or {}, graph, model_type)
    gen_facets = resolve_generation_set(supported, target)

    model_data = load_model(model_path)
    if model_type == "junction":
        junction = model_data.get("junction", {})
        left = (junction.get("left") or {}).get("list_by")
        right = (junction.get("right") or {}).get("list_by")
        if not (left or right):
            # Hard gate: this runs before the per-archetype
            # :ores.*.enabled: override loop below, so an explicit enabled
            # override cannot re-admit messaging for a junction with no
            # declared list read (a stack nothing would subscribe to, and a
            # regeneration that would overwrite a live legacy layer).
            gen_facets = {f for f in gen_facets
                          if f not in _JUNCTION_MESSAGING_FACETS}
    # Per-archetype activation: the entity's ores.* drawer overrides (most-
    # specific wins, archetype depth included) and, for components, the kind
    # discriminator that selects mutually-exclusive variants in one pass.
    overrides = _enabled_overrides(properties or {})
    component_kind = None
    if model_type == "component":
        comp = model_data.setdefault("component", {})
        component_kind = comp.get("kind")
        # Component root on disk relative to projects/ (the nested regrouped
        # layout, e.g. ores.refdata/api), used as {component_dir} so output goes
        # to the real location rather than the dotted name. The model lives at
        # <root>/modeling/component_overview.org.
        try:
            projects_dir = base_dir.resolve().parent
            comp["dir"] = str(
                model_path.resolve().parent.parent.relative_to(projects_dir))
        except (ValueError, OSError):
            pass  # not under projects/ — falls back to dotted full_name
    units: list[dict] = []
    seen: set[str] = set()
    for facet in sorted(gen_facets):
        ts = graph.facet_ts.get(facet, "")
        for arch in graph.facet_archetypes.get(facet, []):
            mts = arch.get("model_types")
            if mts and model_type not in mts:
                continue
            if not kind_matches(arch.get("kinds", []), component_kind):
                continue
            if not is_enabled(arch["address"], facet, ts, overrides,
                              arch.get("default_enabled", True)):
                continue
            template_name, pattern = arch.get("template"), arch.get("output")
            if not template_name or not pattern:
                log.debug("skipping archetype %s — empty template/output",
                          arch.get("address", "?"))
                continue
            try:
                resolved = resolve_output_path(pattern, model_data, model_type)
            except (KeyError, ValueError, TypeError) as exc:
                log.debug("skipping archetype %s — output %r did not resolve: %s",
                          arch.get("address", "?"), pattern, exc)
                continue
            if resolved in seen:
                continue
            seen.add(resolved)
            units.append({
                "template": template_name,
                "output": resolved,
                # data-scope: the dataset-relative payload file this archetype
                # renders from (empty for entity/component archetypes).
                "data_source": arch.get("data_source", ""),
            })
    return units, model_type, model_data


def _generate_single(
    model_path: Path,
    dry_run: bool,
    base_dir: Path,
    address: str | None = None,
    component_mode: bool = False,
) -> int:
    if not model_path.exists():
        log.error("Model file not found: %s", model_path)
        return 1

    try:
        units, model_type, model_data = resolve_targets(
            model_path, base_dir, address=address)
    except ValueError as exc:
        log.error("%s", exc)
        return 1

    if not units:
        graph = load_graph(base_dir / "library" / "templates")
        if not address_supports_model_type(address, model_type, graph):
            # The address can never generate this model type (e.g. a
            # junction model against ores.cpp.qt) — auto-discovered
            # --component runs hit this constantly and it is not a
            # failure, so it stays silent (DEBUG). An explicit
            # single-entity invocation naming an incompatible pair is a
            # caller mistake and still errors.
            if component_mode:
                log.debug(
                    "%s: model type %r is not generated by address %r; "
                    "skipping", model_path.name, model_type, address)
                return 0
            log.error(
                "%s: model type %r is not generated by address %r",
                model_path.name, model_type, address)
            return 1
        # The address supports this model type but nothing is enabled/
        # selected (drawer overrides, default-off facets, ...) — an
        # ordinary empty intersection, not an incompatibility. Warning,
        # not an error (spec): nothing to do.
        log.warning("%s: nothing to generate for %r (model type %r)",
                    model_path.name, address, model_type)
        return 0

    project_root = base_dir.parent.parent
    data_dir = base_dir / "library" / "data"
    templates_dir = base_dir / "library" / "templates"

    # Data-scope (populate/seed) models carry no payload of their own: each
    # archetype names a dataset-relative #+data_source: (the JSON payload),
    # and the dataset model supplies the output prefix. We render each unit
    # from its payload via the EXISTING per-file path so all the legacy
    # enrichment (flag SVGs, currency defaults, dataset-dependency expansion,
    # manifest methodology lift) is reused unchanged.
    is_dataset = model_type == "dataset"
    dataset = model_data.get("dataset", {}) if is_dataset else {}
    dataset_prefix = dataset.get("prefix") or dataset.get("name")
    dataset_dir = model_path.parent

    # A junction or domain_entity with no ** Qt drawer is incompatible with
    # ores.cpp.qt (see generate_from_model's own fail-fast for the same
    # condition). Under an explicit --address ores.cpp.qt request that
    # incompatibility is the whole point of the run and must still error.
    # But under the default, address-less "full supported set" run it used
    # to abort the ENTIRE generate the moment it reached the first Qt unit
    # in iteration order -- silently starving every facet ordered after Qt
    # (repository, service, SQL, ...) of ever being generated for that
    # entity, and for domain_entity there was no fail-fast at all, so a
    # missing Qt drawer instead rendered broken Qt output (empty #include,
    # empty class names) that nothing caught until compile time. Skip just
    # the Qt units instead, so the rest of the supported set still
    # generates and no broken Qt files get written.
    is_junction_no_qt = model_type == "junction" and not model_data.get("junction", {}).get("qt")
    is_domain_entity_no_qt = (
        model_type == "domain_entity" and not model_data.get("domain_entity", {}).get("qt"))
    model_incompatible_with_qt = is_junction_no_qt or is_domain_entity_no_qt
    qt_address_requested = bool(address) and address.startswith("ores.cpp.qt")

    written: list[Path] = []
    for unit in units:
        template_name = unit["template"]
        output_path = project_root / unit["output"]
        is_qt_template = template_name.startswith("cpp_qt_") or template_name.startswith("qt_")
        if model_incompatible_with_qt and is_qt_template and not qt_address_requested:
            log.info(
                "%s: skipping %s -- %s has no ** Qt drawer",
                model_path.name, template_name, model_type)
            continue
        if dry_run:
            print(str(output_path))
            continue
        output_path.parent.mkdir(parents=True, exist_ok=True)
        if is_dataset:
            # Fail fast on a populate archetype that forgot its #+data_source:;
            # otherwise dataset_dir / "" resolves to the directory itself and
            # surfaces a confusing IsADirectoryError downstream.
            if not unit["data_source"]:
                log.error("archetype %s carries no #+data_source:", template_name)
                return 1
            source_path = dataset_dir / unit["data_source"]
            if not source_path.exists():
                log.error("data_source not found for archetype %s: %s",
                          template_name, source_path)
                return 1
            generate_from_model(
                str(source_path),
                data_dir,
                templates_dir,
                output_path.parent,
                # Suppress the model.json batch dispatch: each archetype is a
                # single, already-resolved (template, output) unit here.
                is_processing_batch=True,
                prefix=dataset_prefix,
                target_template=template_name,
                target_output=output_path.name,
            )
        else:
            result = generate_from_model(
                str(model_path),
                data_dir,
                templates_dir,
                output_path.parent,
                is_processing_batch=False,
                target_template=template_name,
                target_output=output_path.name,
            )
            if result:
                # generate_from_model already logged why (e.g. a junction
                # missing the ** Qt drawer its address requires) -- abort
                # rather than hand a nonexistent path to clang_format_files
                # below, which would crash on a file-not-found instead of
                # surfacing the real cause.
                return result
        log.info("Wrote %s", output_path)
        written.append(output_path)

    clang_format_files(written)
    return 0


def cmd_generate(args: Any, base_dir: Path) -> int:
    return _generate_single(
        Path(args.model).resolve(),
        args.dry_run,
        base_dir,
        address=args.address,
    )


def cmd_regenerate(args: Any, base_dir: Path) -> int:
    from .manifest import (  # noqa: PLC0415
        all_components, discover_models, entity_name_from_path, get_component,
    )

    entity_filter = None
    if getattr(args, "entity", None):
        if args.all:
            log.error("--entity is not valid with --all; pass --component instead.")
            return 1
        entity_filter = {n.strip() for n in args.entity.split(",") if n.strip()}

    component_names = all_components() if args.all else [args.component]
    total_errors = 0
    matched_entities: set = set()

    for comp_name in component_names:
        try:
            comp = get_component(comp_name)
        except ValueError as exc:
            log.error("%s", exc)
            total_errors += 1
            continue

        project_root = base_dir.parent.parent
        model_files = discover_models(comp, project_root)
        if not model_files:
            log.warning(
                "No models found for component %r (modeling_dir: %s)",
                comp_name,
                comp.modeling_dir or "(no modeling dir)",
            )
            continue

        if entity_filter is not None:
            selected = []
            for model_path in model_files:
                name = entity_name_from_path(model_path)
                if name in entity_filter:
                    matched_entities.add(name)
                    selected.append(model_path)
            model_files = selected
            if not model_files:
                continue

        log.info(
            "Regenerating %d models for component %r (address: %s)%s...",
            len(model_files),
            comp_name,
            args.address,
            " [dry-run]" if args.dry_run else "",
        )

        for model_path in model_files:
            rc = _generate_single(model_path, args.dry_run, base_dir,
                                  address=args.address, component_mode=True)
            if rc != 0:
                total_errors += 1

    if entity_filter is not None:
        unknown = entity_filter - matched_entities
        if unknown:
            log.error(
                "--entity named %s not found in component %r.",
                ", ".join(sorted(unknown)), args.component,
            )
            total_errors += 1

    if total_errors:
        log.error("%d error(s) during regeneration.", total_errors)
    else:
        log.info("Regeneration complete.")

    return 1 if total_errors else 0
