"""compass codegen entity — entity-level codegen sub-commands.

Imported lazily by cmd_codegen in compass.py when 'entity' is the first
argument. All heavy imports (codegen package) are deferred until run time.
"""

import re
import subprocess
import sys
from pathlib import Path

# Metatypes that represent full domain objects; shown by default in 'list'.
_PRIMARY_METATYPES = frozenset({"domain_entity", "junction"})

_ORG_ID_RE = re.compile(r"^:ID:\s+(\S+)", re.MULTILINE)

_STATUS_ICON = {"ok": "✅", "STALE": "⚠️ ", "MISSING": "❌", "?": "❓"}


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def _read_org_id(path: Path) -> str:
    """Return the org-roam :ID: from a model file, or '' if absent."""
    try:
        head = path.read_text(encoding="utf-8", errors="replace")[:2048]
        m = _ORG_ID_RE.search(head)
        return m.group(1) if m else ""
    except OSError:
        return ""


def _entity_name_from_path(path: Path) -> str:
    """Extract a human-readable entity name from a model file path.

    ores.refdata.country.org        → country
    ores.refdata.book_status.org    → book_status
    country_domain_entity.json      → country
    audit_record_field_group.org    → audit_record
    """
    stem = path.stem
    for suffix in (
        "_domain_entity", "_field_group", "_junction",
        "_table", "_lookup_entity", "_enum", "_component", "_service_registry",
    ):
        if stem.endswith(suffix):
            stem = stem[: -len(suffix)]
            break
    if "." in stem:
        stem = stem.split(".")[-1]
    return stem


def _discover_all(base_dir: Path, project_root: Path):
    """Yield (model_path, metatype, component_name, entity_name) for every
    model file across all components.

    Passes apply_exclusions=False: exclude_org_types scopes bulk `codegen
    regenerate --component` regeneration only (see discover_models), and
    single-entity commands (list/generate/show/diff/archetypes --entity)
    must still resolve every model, including excluded types like
    junctions.
    """
    from codegen.manifest import all_components, discover_models, get_component  # noqa: PLC0415
    from codegen.core import get_model_type  # noqa: PLC0415

    for comp_name in all_components():
        comp = get_component(comp_name)
        for model_path in discover_models(comp, project_root, apply_exclusions=False):
            metatype = get_model_type(model_path.name, model_path)
            entity_name = _entity_name_from_path(model_path)
            yield model_path, metatype, comp_name, entity_name


def _resolve_entity(name_or_id: str, base_dir: Path, project_root: Path):
    """Return (model_path, metatype, component_name, entity_name) for the
    entity matching name_or_id (by entity name or org-roam ID prefix).

    Deduplicates by model path (same file under multiple components = one
    result). When multiple distinct paths remain, prefers primary metatypes
    (domain_entity, junction) over meta-entity metatypes. Raises SystemExit
    on no match or genuine ambiguity.

    Name matching is case-sensitive (entity names are authoritative identifiers);
    ID prefix matching is case-insensitive (UUIDs have no meaningful case)."""
    needle = name_or_id.upper()
    raw = []
    for model_path, metatype, comp_name, entity_name in _discover_all(base_dir, project_root):
        if entity_name == name_or_id:
            raw.append((model_path, metatype, comp_name, entity_name))
            continue
        oid = _read_org_id(model_path).upper()
        if oid and oid.startswith(needle):
            raw.append((model_path, metatype, comp_name, entity_name))

    # Deduplicate by model path — keep first comp_name seen per path.
    seen: dict = {}
    for item in raw:
        path = item[0]
        if path not in seen:
            seen[path] = item
    matches = list(seen.values())

    if not matches:
        print(f"❌ No entity found matching {name_or_id!r}.", file=sys.stderr)
        sys.exit(1)
    if len(matches) == 1:
        return matches[0]

    # Prefer primary metatypes if present.
    primary = [m for m in matches if m[1] in _PRIMARY_METATYPES]
    if len(primary) == 1:
        return primary[0]
    if len(primary) > 1:
        matches = primary  # narrow to primaries, then report ambiguity below

    lines = "\n".join(f"  {m[3]} ({m[1]}) — {m[0]}" for m in matches)
    print(f"❌ Ambiguous: {len(matches)} entities match {name_or_id!r}:\n{lines}",
          file=sys.stderr)
    sys.exit(1)


def _output_paths_for_entity(model_path: Path, base_dir: Path, project_root: Path,
                              address=None) -> list:
    """(output_path, template_path) pairs for an entity, via the shared
    codegen resolver — no duplicated resolution logic here."""
    from codegen.generate import resolve_targets  # noqa: PLC0415
    units, _, _ = resolve_targets(model_path, base_dir, address=address)
    templates_dir = base_dir / "library" / "templates"
    return [(project_root / u["output"], templates_dir / u["template"]) for u in units]


# ---------------------------------------------------------------------------
# Sub-command handlers
# ---------------------------------------------------------------------------

def _cmd_list(args, base_dir: Path, project_root: Path) -> int:
    groups: dict = {}
    for model_path, metatype, comp_name, entity_name in _discover_all(base_dir, project_root):
        groups.setdefault(metatype, []).append((entity_name, comp_name, model_path))

    metatype_filter = getattr(args, "metatype", None)
    show_all = getattr(args, "all", False)

    if metatype_filter:
        display_types = [metatype_filter] if metatype_filter in groups else []
    elif show_all:
        display_types = sorted(groups)
    else:
        display_types = sorted(t for t in groups if t in _PRIMARY_METATYPES)

    if not display_types:
        label = metatype_filter or ("all" if show_all else "entity/junction")
        print(f"No entities found for metatype filter: {label}")
        return 0

    for metatype in display_types:
        entries = sorted(groups[metatype], key=lambda e: (e[1], e[0]))
        tier = "entity" if metatype in _PRIMARY_METATYPES else "meta-entity"
        print(f"\n{metatype}  [{tier}]  ({len(entries)})")
        print(f"  {'name':<30} {'ID':<10} {'component':<20} model path")
        print(f"  {'-'*30} {'-'*10} {'-'*20} ----------")
        for entity_name, comp_name, model_path in entries:
            uuid = _read_org_id(model_path)
            short_id = uuid[:8] if uuid else ""
            rel = model_path.relative_to(project_root)
            print(f"  {entity_name:<30} {short_id:<10} {comp_name:<20} {rel}")
    print()
    return 0


def _diff_entity(model_path: Path, base_dir: Path, project_root: Path,
                 address=None) -> int:
    """Generate to a tempdir via the shared resolver and print a unified diff
    against the on-disk files."""
    import difflib  # noqa: PLC0415
    import io  # noqa: PLC0415
    import logging  # noqa: PLC0415
    import tempfile  # noqa: PLC0415
    from contextlib import redirect_stdout  # noqa: PLC0415
    from codegen.core import generate_from_model  # noqa: PLC0415
    from codegen.generate import clang_format_files, resolve_targets  # noqa: PLC0415

    units, _, _ = resolve_targets(model_path, base_dir, address=address)
    data_dir = base_dir / "library" / "data"
    templates_dir = base_dir / "library" / "templates"
    has_diff = False

    with tempfile.TemporaryDirectory() as tmpdir:
        tmp_root = Path(tmpdir)
        # clang-format only discovers .clang-format by walking up from the
        # formatted file; a bare tempdir has no such ancestor, so it silently
        # falls back to LLVM's default style (2-space indent) instead of the
        # repo's 4-space style. Seed the tempdir root with a copy so the
        # diff reflects real drift, not this formatting artefact.
        repo_clang_format = project_root / ".clang-format"
        if repo_clang_format.exists():
            (tmp_root / ".clang-format").write_bytes(repo_clang_format.read_bytes())
        logging.disable(logging.CRITICAL)
        try:
            for unit in units:
                template_name = unit["template"]
                output_path = project_root / unit["output"]
                rel = output_path.relative_to(project_root)
                tmp_path = tmp_root / rel
                tmp_path.parent.mkdir(parents=True, exist_ok=True)
                with redirect_stdout(io.StringIO()):
                    generate_from_model(
                        str(model_path), data_dir, templates_dir,
                        tmp_path.parent,
                        is_processing_batch=False,
                        target_template=template_name,
                        target_output=tmp_path.name,
                    )
                if not tmp_path.exists():
                    continue
                # Match the real generate path: clang-format C++ output so
                # the diff reflects content, not template whitespace.
                clang_format_files([tmp_path])

                rel_str = str(rel)
                if not output_path.exists():
                    new_lines = tmp_path.read_text(encoding="utf-8", errors="replace").splitlines(keepends=True)
                    chunk = list(difflib.unified_diff(
                        [], new_lines, fromfile="/dev/null", tofile=rel_str))
                else:
                    orig = output_path.read_text(encoding="utf-8", errors="replace").splitlines(keepends=True)
                    new = tmp_path.read_text(encoding="utf-8", errors="replace").splitlines(keepends=True)
                    chunk = list(difflib.unified_diff(
                        orig, new, fromfile=f"a/{rel_str}", tofile=f"b/{rel_str}"))
                if chunk:
                    sys.stdout.writelines(chunk)
                    has_diff = True
        finally:
            logging.disable(logging.NOTSET)

    if not has_diff:
        print("✅ No differences.")
    return 1 if has_diff else 0


def _cmd_generate(args, base_dir: Path, project_root: Path) -> int:
    # _generate_single is a private but stable entry point; a public alias is a future ores.codegen task.
    from codegen.generate import _generate_single  # noqa: PLC0415

    model_path, metatype, comp_name, entity_name = _resolve_entity(
        args.entity, base_dir, project_root)

    address = getattr(args, "address", None)
    dry_run = getattr(args, "dry_run", False)
    diff_mode = getattr(args, "diff", False)
    scope = address or "supported set"

    if diff_mode:
        print(f"Diffing {entity_name} ({metatype}, {comp_name}) — {scope}")
        return _diff_entity(model_path, base_dir, project_root, address=address)

    print(f"{'[dry-run] ' if dry_run else ''}Generating {entity_name} "
          f"({metatype}, {comp_name}) — {scope}")
    return _generate_single(model_path, dry_run, base_dir, address=address)


def _cmd_show(args, base_dir: Path, project_root: Path) -> int:
    model_path, metatype, comp_name, entity_name = _resolve_entity(
        args.entity, base_dir, project_root)

    address = getattr(args, "address", None)
    pairs = _output_paths_for_entity(model_path, base_dir, project_root, address=address)

    if not pairs:
        print(f"No generated files found for {entity_name!r} ({metatype}).")
        return 0

    print(f"\n{entity_name}  ({metatype}, {comp_name})\n")

    for output_path, template_path in sorted(pairs, key=lambda p: str(p[0])):
        rel = output_path.relative_to(project_root) if output_path.is_relative_to(project_root) else output_path
        if not output_path.exists():
            status = "MISSING"
        else:
            try:
                out_mtime = output_path.stat().st_mtime
                tpl_mtime = template_path.stat().st_mtime if template_path.exists() else 0
                model_mtime = model_path.stat().st_mtime if model_path.exists() else 0
                src_mtime = max(tpl_mtime, model_mtime)
                status = "STALE" if out_mtime < src_mtime else "ok"
            except OSError:
                status = "?"
        icon = _STATUS_ICON.get(status, status)
        print(f"  {icon}  {rel}  ←  {template_path.name}")
    print()
    return 0


def _cmd_diff(args, base_dir: Path, project_root: Path) -> int:
    model_path, metatype, comp_name, entity_name = _resolve_entity(
        args.entity, base_dir, project_root)

    address = getattr(args, "address", None)
    pairs = _output_paths_for_entity(model_path, base_dir, project_root, address=address)

    existing = [str(p.relative_to(project_root)) for p, _ in pairs if p.exists()]
    if not existing:
        print(f"No generated files on disk for {entity_name!r}.")
        return 0

    result = subprocess.run(
        ["git", "diff", "--"] + existing,
        cwd=str(project_root),
    )
    return result.returncode


def _cmd_archetypes(args, base_dir: Path, project_root: Path) -> int:
    from codegen.core import get_model_type, load_model  # noqa: PLC0415
    from codegen.generate import _read_drawer_properties  # noqa: PLC0415
    from codegen.physical_space import (  # noqa: PLC0415
        _enabled_overrides, compute_target_set, is_enabled_with_reason,
        kind_matches, load_graph,
    )
    _enable_reason = is_enabled_with_reason

    graph = load_graph(base_dir / "library" / "templates")
    address = getattr(args, "address", None)
    entity = getattr(args, "entity", None)

    # compute_target_set only resolves TS- and facet-level addresses (the
    # graph's discovery roots); an archetype-level address narrows further,
    # to that one archetype within its facet — resolved here rather than in
    # physical_space.py since it is a presentation-only narrowing, not a
    # generation-set decision.
    target_archetype = None
    try:
        target_facets = compute_target_set(address, graph)
    except ValueError:
        target_archetype = next(
            (arch["address"] for archs in graph.facet_archetypes.values()
             for arch in archs if arch["address"] == address),
            None,
        )
        if target_archetype is None:
            print(f"❌ unknown address: {address!r}", file=sys.stderr)
            return 1
        owning_facet = next(
            f for f, archs in graph.facet_archetypes.items()
            if any(a["address"] == target_archetype for a in archs))
        target_facets = frozenset({owning_facet})

    model_type = None
    overrides: dict = {}
    component_kind = None
    if entity:
        model_path, _metatype, comp_name, entity_name = _resolve_entity(
            entity, base_dir, project_root)
        properties = _read_drawer_properties(model_path)
        model_type = get_model_type(model_path.name, model_path)
        overrides = _enabled_overrides(properties or {})
        # Component entities carry a mutually-exclusive kind discriminator
        # (api/core/flat/service/...) that generate.py's resolve_targets
        # hard-filters archetypes by; without it here, kind-restricted
        # archetypes (e.g. ores.cpp.service-app.main, component_kind:
        # service) would show ✅ for every component regardless of its
        # actual kind.
        if model_type == "component":
            component_kind = load_model(model_path).get("component", {}).get("kind")

        print(f"\nSupported set for: {entity_name}  ({model_type}, {comp_name})")
        bindings = sorted(
            f":{k}.enabled: {str(v).lower()}" for k, v in overrides.items())
        print(f"  ores.* bindings: {', '.join(bindings) if bindings else '(none)'}\n")
    else:
        print("\nPhysical space catalogue\n")

    for ts, facets in graph.ts_facets.items():
        shown_facets = [f for f in facets if f in target_facets]
        if not shown_facets:
            continue

        if model_type is None:
            print(f"{ts}  [technical space]")
        else:
            ts_supported = any(
                (not graph.facet_model_types.get(f) or model_type in graph.facet_model_types[f])
                and _enable_reason(f, f, ts, overrides, graph.facet_default.get(f, True))[0]
                for f in shown_facets
            )
            icon = "✅" if ts_supported else "❌"
            suffix = " [technical space — in supported set]" if ts_supported \
                else " [technical space — no admissible facet]"
            print(f"{ts}  {icon}{suffix}")

        for facet in shown_facets:
            mts = graph.facet_model_types.get(facet, [])
            mts_label = " ".join(mts) if mts else "(any)"

            if model_type is None:
                print(f"  {facet}  [facet — model types: {mts_label}]")
            else:
                admissible = not mts or model_type in mts
                if not admissible:
                    print(f"  {facet}  ❌ [facet — excluded, model type "
                          f"'{model_type}' not admitted (model types: {mts_label})]")
                else:
                    enabled, reason_key = _enable_reason(
                        facet, facet, ts, overrides, graph.facet_default.get(facet, True))
                    if enabled:
                        print(f"  {facet}  ✅ [facet — model types: {mts_label}]")
                    else:
                        why = f"by :{reason_key}.enabled: false" if reason_key \
                            else "by facet default (#+default: disabled)"
                        print(f"  {facet}  ❌ [facet — excluded {why}]")

            for arch in graph.facet_archetypes.get(facet, []):
                addr, template = arch["address"], arch.get("template", "")
                if target_archetype and addr != target_archetype:
                    continue
                if model_type is None:
                    print(f"    {addr:<34} {template}")
                    continue

                # load_graph already folds the facet's model_types into an
                # archetype's when the archetype declares none of its own
                # (physical_space.load_graph), so arch["model_types"] alone
                # is always the effective set — no separate `or mts` needed.
                arch_mts = arch.get("model_types")
                if arch_mts and model_type not in arch_mts:
                    print(f"    {addr:<34} ❌  {template}  "
                          f"(model type '{model_type}' not admitted)")
                    continue
                if not admissible:
                    print(f"    {addr:<34} ❌  {template}")
                    continue
                if not kind_matches(arch.get("kinds", []), component_kind):
                    print(f"    {addr:<34} ❌  {template}  "
                          f"(component kind {component_kind!r} not admitted)")
                    continue

                arch_enabled, arch_reason = _enable_reason(
                    addr, facet, ts, overrides, arch.get("default_enabled", True))
                # A disabled facet cascades to every archetype under it.
                if not enabled:
                    arch_enabled, arch_reason = False, reason_key
                if arch_enabled:
                    print(f"    {addr:<34} ✅  {template}")
                else:
                    why = f"excluded by :{arch_reason}.enabled: false" if arch_reason \
                        else "excluded by default (#+default: disabled)"
                    print(f"    {addr:<34} ❌  {template}  ({why})")
        print()
    return 0


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

def run(argv, base_dir: Path, project_root: Path) -> int:
    import argparse as _ap  # noqa: PLC0415

    ap = _ap.ArgumentParser(
        prog="compass codegen entity",
        description="Entity-level codegen operations.",
    )
    sub = ap.add_subparsers(dest="subcmd", required=True)

    # list
    lp = sub.add_parser("list", help="List entities grouped by metatype.")
    lp.add_argument("--all", action="store_true",
                    help="Include meta-entity metatypes (enum, field_group, …)")
    lp.add_argument("--type", dest="metatype", metavar="METATYPE",
                    help="Show only entities of this metatype.")

    # generate
    gp = sub.add_parser("generate", aliases=["gen"],
                         help="Regenerate an entity's full supported set, or one address.")
    gp.add_argument("entity", help="Entity name or org-roam ID prefix.")
    gp.add_argument("--address", metavar="ADDRESS",
                    help="Physical-space address to generate (e.g. ores.sql.schema, "
                         "ores.cpp.qt); omit for the entity's full supported set.")
    gmode = gp.add_mutually_exclusive_group()
    gmode.add_argument("--dry-run", action="store_true",
                       help="Print output paths without writing.")
    gmode.add_argument("--diff", action="store_true",
                       help="Generate to a temp dir and show unified diff against on-disk files.")

    # show
    sp = sub.add_parser("show", help="Show generated files with staleness.")
    sp.add_argument("entity", help="Entity name or org-roam ID prefix.")
    sp.add_argument("--address", metavar="ADDRESS",
                    help="Restrict to a physical-space address.")

    # diff
    dp = sub.add_parser("diff", help="Git diff of all generated files for an entity.")
    dp.add_argument("entity", help="Entity name or org-roam ID prefix.")
    dp.add_argument("--address", metavar="ADDRESS",
                    help="Restrict to a physical-space address.")

    # archetypes
    ap_ = sub.add_parser(
        "archetypes",
        help="List the physical-space catalogue, or an entity's supported set.")
    ap_.add_argument("--entity", metavar="NAME",
                     help="Show the supported set for this entity, with "
                          "per-archetype ✅/❌ status and exclusion reasons.")
    ap_.add_argument("--address", metavar="ADDRESS",
                     help="Restrict to a technical space, facet, or archetype "
                          "(e.g. ores.cpp, ores.cpp.qt, ores.cpp.qt.controller_header).")

    args = ap.parse_args(argv)

    # argparse sets args.subcmd to the literal string the user typed (alias included),
    # so both "generate" and "gen" must appear as distinct keys.
    dispatch = {
        "list":       _cmd_list,
        "generate":   _cmd_generate,
        "gen":        _cmd_generate,
        "show":       _cmd_show,
        "diff":       _cmd_diff,
        "archetypes": _cmd_archetypes,
    }
    return dispatch[args.subcmd](args, base_dir, project_root)
