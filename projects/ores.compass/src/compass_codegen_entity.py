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
    model file across all components."""
    from codegen.manifest import all_components, discover_models, get_component  # noqa: PLC0415
    from codegen.core import get_model_type  # noqa: PLC0415

    for comp_name in all_components():
        comp = get_component(comp_name)
        for model_path in discover_models(comp, project_root):
            metatype = get_model_type(model_path.name)
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


def _applicable_profiles(metatype: str, profiles: dict) -> list:
    """Return names of individual facets (not groups) that apply to metatype."""
    result = []
    for name, profile in profiles.items():
        if "includes" in profile:
            continue  # skip facet_groups
        if metatype in profile.get("model_types", []):
            result.append(name)
    return sorted(result)


def _output_paths_for_entity(model_path: Path, base_dir: Path, project_root: Path,
                              profile_filter=None) -> list:
    """Return list of (output_path, template_path) for an entity across all
    applicable profiles (or the single profile given by profile_filter)."""
    from codegen.core import (  # noqa: PLC0415
        get_model_type, load_profiles, resolve_profile_templates,
        resolve_output_path, load_model, validate_profile_for_model,
    )
    profiles = load_profiles(base_dir)
    metatype = get_model_type(model_path.name)
    model_data = load_model(model_path)
    templates_dir = base_dir / "library" / "templates"

    if profile_filter:
        profile_names = [profile_filter]
    else:
        profile_names = _applicable_profiles(metatype, profiles)

    seen: set = set()
    results = []
    for prof_name in profile_names:
        is_valid, _ = validate_profile_for_model(prof_name, profiles, metatype)
        if not is_valid:
            continue
        for tmpl_info in resolve_profile_templates(prof_name, profiles, metatype):
            if not isinstance(tmpl_info, dict):
                continue
            output_pattern = tmpl_info.get("output")
            template_name = tmpl_info.get("template")
            if not output_pattern or not template_name:
                continue
            try:
                resolved = resolve_output_path(output_pattern, model_data, metatype)
            except (KeyError, ValueError, TypeError):
                continue
            output_path = project_root / resolved
            if output_path in seen:
                continue
            seen.add(output_path)
            template_path = templates_dir / template_name
            results.append((output_path, template_path))
    return results


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


def _diff_profiles(model_path: Path, profile_names: list, base_dir: Path,
                   project_root: Path) -> int:
    """Generate all profiles to a tempdir and print a unified diff against on-disk files."""
    import difflib  # noqa: PLC0415
    import io  # noqa: PLC0415
    import logging  # noqa: PLC0415
    import tempfile  # noqa: PLC0415
    from contextlib import redirect_stdout  # noqa: PLC0415
    from codegen.core import (  # noqa: PLC0415
        get_model_type, load_profiles, validate_profile_for_model,
        resolve_profile_templates, resolve_output_path, load_model,
        generate_from_model,
    )

    profiles = load_profiles(base_dir)
    metatype = get_model_type(model_path.name)
    model_data = load_model(model_path)
    data_dir = base_dir / "library" / "data"
    templates_dir = base_dir / "library" / "templates"

    seen: set = set()
    has_diff = False

    with tempfile.TemporaryDirectory() as tmpdir:
        tmp_root = Path(tmpdir)
        logging.disable(logging.CRITICAL)
        try:
            for prof_name in profile_names:
                is_valid, _ = validate_profile_for_model(prof_name, profiles, metatype)
                if not is_valid:
                    continue
                for tmpl_info in resolve_profile_templates(prof_name, profiles, metatype):
                    if not isinstance(tmpl_info, dict):
                        continue
                    output_pattern = tmpl_info.get("output")
                    template_name = tmpl_info.get("template")
                    if not output_pattern or not template_name:
                        continue
                    try:
                        resolved = resolve_output_path(output_pattern, model_data, metatype)
                    except (KeyError, ValueError, TypeError):
                        continue
                    output_path = project_root / resolved
                    if output_path in seen:
                        continue
                    seen.add(output_path)

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
    from codegen.core import get_model_type, load_profiles  # noqa: PLC0415

    model_path, metatype, comp_name, entity_name = _resolve_entity(
        args.entity, base_dir, project_root)

    profiles = load_profiles(base_dir)
    profile_filter = getattr(args, "profile", None)
    dry_run = getattr(args, "dry_run", False)
    diff_mode = getattr(args, "diff", False)

    if profile_filter:
        profile_names = [profile_filter]
    else:
        profile_names = _applicable_profiles(metatype, profiles)

    if not profile_names:
        print(f"❌ No applicable profiles for metatype {metatype!r}.", file=sys.stderr)
        return 1

    if diff_mode:
        print(f"Diffing {entity_name} ({metatype}, {comp_name})"
              f" — {len(profile_names)} profile(s): {', '.join(profile_names)}")
        return _diff_profiles(model_path, profile_names, base_dir, project_root)

    print(f"{'[dry-run] ' if dry_run else ''}Generating {entity_name} "
          f"({metatype}, {comp_name}) — {len(profile_names)} profile(s): "
          f"{', '.join(profile_names)}")

    total_errors = 0
    for prof_name in profile_names:
        rc = _generate_single(model_path, prof_name, dry_run, base_dir)
        if rc != 0:
            total_errors += 1

    if total_errors:
        print(f"❌ {total_errors} profile(s) failed.", file=sys.stderr)
    return 1 if total_errors else 0


def _cmd_show(args, base_dir: Path, project_root: Path) -> int:
    model_path, metatype, comp_name, entity_name = _resolve_entity(
        args.entity, base_dir, project_root)

    profile_filter = getattr(args, "profile", None)
    pairs = _output_paths_for_entity(model_path, base_dir, project_root, profile_filter)

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

    profile_filter = getattr(args, "profile", None)
    pairs = _output_paths_for_entity(model_path, base_dir, project_root, profile_filter)

    existing = [str(p.relative_to(project_root)) for p, _ in pairs if p.exists()]
    if not existing:
        print(f"No generated files on disk for {entity_name!r}.")
        return 0

    result = subprocess.run(
        ["git", "diff", "--"] + existing,
        cwd=str(project_root),
    )
    return result.returncode


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
                         help="Regenerate all applicable profiles for an entity.")
    gp.add_argument("entity", help="Entity name or org-roam ID prefix.")
    gp.add_argument("--profile", metavar="PROFILE",
                    help="Restrict to one profile instead of running all.")
    gmode = gp.add_mutually_exclusive_group()
    gmode.add_argument("--dry-run", action="store_true",
                       help="Print output paths without writing.")
    gmode.add_argument("--diff", action="store_true",
                       help="Generate to a temp dir and show unified diff against on-disk files.")

    # show
    sp = sub.add_parser("show", help="Show generated files with staleness.")
    sp.add_argument("entity", help="Entity name or org-roam ID prefix.")
    sp.add_argument("--profile", metavar="PROFILE",
                    help="Restrict to one profile.")

    # diff
    dp = sub.add_parser("diff", help="Git diff of all generated files for an entity.")
    dp.add_argument("entity", help="Entity name or org-roam ID prefix.")
    dp.add_argument("--profile", metavar="PROFILE",
                    help="Restrict to one profile.")

    args = ap.parse_args(argv)

    # argparse sets args.subcmd to the literal string the user typed (alias included),
    # so both "generate" and "gen" must appear as distinct keys.
    dispatch = {
        "list":     _cmd_list,
        "generate": _cmd_generate,
        "gen":      _cmd_generate,
        "show":     _cmd_show,
        "diff":     _cmd_diff,
    }
    return dispatch[args.subcmd](args, base_dir, project_root)
