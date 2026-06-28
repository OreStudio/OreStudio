import json
import logging
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
    compute_supported_set,
    compute_target_set,
    is_enabled,
    kind_matches,
    load_graph,
    resolve_generation_set,
)

log = logging.getLogger(__name__)

# Backward-compat shim: legacy --profile names were *curated facet sets*
# (facet_catalogue groups), not address subtrees, so map each to its exact
# set of facet addresses to preserve byte-identical output. Removed in
# stage 6 once every caller passes --address. A value not listed here is
# treated as a physical-space address and expanded by the graph.
_CPP_CORE = {"ores.cpp.domain", "ores.cpp.generator", "ores.cpp.repository",
             "ores.cpp.service", "ores.cpp.protocol"}
_PROFILE_TO_FACETS = {
    "sql": {"ores.sql.schema"},
    "non-temporal-sql": {"ores.sql.non-temporal"},
    "domain": {"ores.cpp.domain"},
    "generator": {"ores.cpp.generator"},
    "repository": {"ores.cpp.repository"},
    "service": {"ores.cpp.service"},
    "protocol": {"ores.cpp.protocol"},
    "nats-eventing": {"ores.cpp.nats-eventing"},
    "nats-handler": {"ores.cpp.nats-handler"},
    "qt": {"ores.cpp.qt"},
    "enum": {"ores.cpp.enum"},
    "field-group": {"ores.cpp.field-group"},
    "non-temporal-domain": {"ores.cpp.non-temporal-domain"},
    "non-temporal-repository": {"ores.cpp.non-temporal-repository"},
    "all-cpp": set(_CPP_CORE),
    "all": _CPP_CORE | {"ores.sql.schema"},
    "non-temporal": {"ores.sql.non-temporal", "ores.cpp.non-temporal-domain",
                     "ores.cpp.non-temporal-repository"},
}

# Filter for org files in a component's modeling/ dir: only files whose
# frontmatter declares a codegen model type are picked up. Other org
# files (overviews, knowledge docs, plantuml source) are skipped.
from .manifest import is_codegen_entity_org as _is_codegen_entity_org  # noqa: E402


def resolve_targets(
    model_path: Path,
    base_dir: Path,
    *,
    profile: str | None = None,
    address: str | None = None,
    properties: dict[str, Any] | None = None,
) -> tuple[list[dict], str, dict]:
    """Traverse the physical-space graph; return what to generate for a model.

    THE single resolver — used by the codegen CLI and the compass wrapper, so
    there is one place that maps (model, selector) → archetypes. Selector
    precedence: ``address`` (real physical-space address) > ``profile``
    (legacy compat shim) > neither (the entity's full supported set).

    Returns ``(units, model_type, model_data)`` where each unit is
    ``{"template": <name>, "output": <project-root-relative path>}``.
    Raises ``ValueError`` on an unknown address.
    """
    graph = load_graph(base_dir / "library" / "templates")
    model_type = get_model_type(model_path.name, model_path)

    if address:
        target = compute_target_set(address, graph)
    elif profile in _PROFILE_TO_FACETS:
        log.warning("--profile %r is deprecated; use --address (e.g. %s). "
                    "The shim is removed once callers migrate (task B11).",
                    profile, " ".join(sorted(_PROFILE_TO_FACETS[profile])))
        target = frozenset(_PROFILE_TO_FACETS[profile])
    elif profile:
        target = compute_target_set(profile, graph)     # profile is an address
    else:
        target = compute_target_set(None, graph)         # all facets

    # TODO(B5): read the entity's :ores.*.enabled: drawer for per-entity
    # disables; empty props => S_e is every model-type-admissible facet.
    supported = compute_supported_set(properties or {}, graph, model_type)
    gen_facets = resolve_generation_set(supported, target)

    model_data = load_model(model_path)
    # Per-archetype activation: the entity's ores.* drawer overrides (most-
    # specific wins, archetype depth included) and, for components, the kind
    # discriminator that selects mutually-exclusive variants in one pass.
    overrides = _enabled_overrides(properties or {})
    component_kind = None
    if model_type == "component":
        component_kind = (model_data.get("component") or {}).get("kind")
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
            units.append({"template": template_name, "output": resolved})
    return units, model_type, model_data


def _generate_single(
    model_path: Path,
    profile: str,
    dry_run: bool,
    base_dir: Path,
    address: str | None = None,
) -> int:
    if not model_path.exists():
        log.error("Model file not found: %s", model_path)
        return 1

    try:
        units, model_type, _ = resolve_targets(
            model_path, base_dir, profile=profile, address=address)
    except ValueError as exc:
        log.error("%s", exc)
        return 1

    if not units:
        # Empty intersection is a warning, not an error (spec): nothing to do.
        log.warning("%s: nothing to generate for %r (model type %r)",
                    model_path.name, address or profile, model_type)
        return 0

    project_root = base_dir.parent.parent
    data_dir = base_dir / "library" / "data"
    templates_dir = base_dir / "library" / "templates"

    for unit in units:
        template_name = unit["template"]
        output_path = project_root / unit["output"]
        if dry_run:
            print(str(output_path))
            continue
        output_path.parent.mkdir(parents=True, exist_ok=True)
        generate_from_model(
            str(model_path),
            data_dir,
            templates_dir,
            output_path.parent,
            is_processing_batch=False,
            target_template=template_name,
            target_output=output_path.name,
        )
        log.info("Wrote %s", output_path)

    return 0


def cmd_generate(args: Any, base_dir: Path) -> int:
    return _generate_single(
        Path(args.model).resolve(),
        args.profile,
        args.dry_run,
        base_dir,
        address=getattr(args, "address", None),
    )


def cmd_regenerate(args: Any, base_dir: Path) -> int:
    from .manifest import (  # noqa: PLC0415
        all_components, discover_models, get_component,
    )

    component_names = all_components() if args.all else [args.component]
    total_errors = 0

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
                "No models found for component %r (looked in %s and %s)",
                comp_name, comp.models_dir, comp.modeling_dir or "(no modeling dir)",
            )
            continue

        log.info(
            "Regenerating %d models for component %r (profile: %s)%s...",
            len(model_files),
            comp_name,
            args.profile,
            " [dry-run]" if args.dry_run else "",
        )

        for model_path in model_files:
            rc = _generate_single(model_path, args.profile, args.dry_run, base_dir,
                                  address=getattr(args, "address", None))
            if rc != 0:
                total_errors += 1

    if total_errors:
        log.error("%d error(s) during regeneration.", total_errors)
    else:
        log.info("Regeneration complete.")

    return 1 if total_errors else 0
