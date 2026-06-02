import json
import logging
import re
import sys
from pathlib import Path
from typing import Any

log = logging.getLogger(__name__)

_SAFE_PROFILES_FOR_ENTITY = frozenset({"sql"})

# Filter for org files in a component's modeling/ dir: only files whose
# frontmatter declares a codegen model type are picked up. Other org
# files (overviews, knowledge docs, plantuml source) are skipped.
_ORG_TYPE_RE = re.compile(r"^#\+type:\s*(\S+)\s*$", re.MULTILINE | re.IGNORECASE)
_CODEGEN_ORG_TYPES = frozenset({
    "ores.codegen.entity",
    "ores.codegen.field_group",
    "ores.codegen.junction",
    "ores.codegen.table",
    "ores.codegen.lookup_entity",
})


def _is_codegen_entity_org(path: Path) -> bool:
    """True if the file's frontmatter declares it a codegen org-model."""
    with path.open(encoding="utf-8", errors="replace") as f:
        head = f.read(4096)
    match = _ORG_TYPE_RE.search(head)
    return bool(match and match.group(1) in _CODEGEN_ORG_TYPES)


def _import_generator(base_dir: Path) -> Any:
    src_dir = base_dir / "src"
    if str(src_dir) not in sys.path:
        sys.path.insert(0, str(src_dir))
    import generator  # noqa: PLC0415
    return generator


def _generate_single(
    model_path: Path,
    profile: str,
    dry_run: bool,
    base_dir: Path,
) -> int:
    gen = _import_generator(base_dir)

    if not model_path.exists():
        log.error("Model file not found: %s", model_path)
        return 1

    profiles = gen.load_profiles(base_dir)
    model_type = gen.get_model_type(model_path.name)

    # Refuse --profile all for schema/table models: running the all profile
    # silently overwrites production SQL with the wrong template when both
    # a SQL model (_table.json / _entity.json) and a _domain_entity.json exist
    # for the same entity.
    if profile == "all" and model_type in ("schema", "table"):
        log.error(
            "--profile all is not allowed for SQL models (_table.json or _entity.json). "
            "Use --profile sql or --profile all-cpp explicitly to prevent "
            "accidental SQL overwrites."
        )
        return 1

    is_valid, error = gen.validate_profile_for_model(profile, profiles, model_type)
    if not is_valid:
        log.error("%s", error)
        return 1

    templates = gen.resolve_profile_templates(profile, profiles, model_type)
    if not templates:
        log.error(
            "Profile %r has no applicable templates for model type %r",
            profile,
            model_type,
        )
        return 1

    if str(model_path).endswith(".org"):
        from codegen.org_loader import (
            load_org_model,
            load_org_field_group_model,
            load_org_junction_model,
            load_org_table_model,
            load_org_lookup_entity_model,
        )
        if str(model_path).endswith("_field_group.org"):
            model_data = load_org_field_group_model(model_path)
        elif str(model_path).endswith("_junction.org"):
            model_data = load_org_junction_model(model_path)
        elif str(model_path).endswith("_table.org"):
            model_data = load_org_table_model(model_path)
        elif str(model_path).endswith("_lookup_entity.org"):
            model_data = load_org_lookup_entity_model(model_path)
        else:
            model_data = load_org_model(model_path)
    else:
        with open(model_path, encoding="utf-8") as f:
            model_data = json.load(f)

    project_root = base_dir.parent.parent
    data_dir = base_dir / "library" / "data"
    templates_dir = base_dir / "library" / "templates"

    for tmpl_info in templates:
        template_name = tmpl_info.get("template") if isinstance(tmpl_info, dict) else tmpl_info
        output_pattern = tmpl_info.get("output") if isinstance(tmpl_info, dict) else None

        if output_pattern:
            resolved = gen.resolve_output_path(output_pattern, model_data, model_type)
            output_path = project_root / resolved
        else:
            output_path = None

        if dry_run:
            label = str(output_path) if output_path else f"<output>/{template_name}"
            print(label)
            continue

        if output_path:
            output_path.parent.mkdir(parents=True, exist_ok=True)
            gen.generate_from_model(
                str(model_path),
                data_dir,
                templates_dir,
                output_path.parent,
                is_processing_batch=False,
                target_template=template_name,
                target_output=output_path.name,
            )
            log.info("Wrote %s", output_path)
        else:
            output_dir = base_dir / "output"
            output_dir.mkdir(parents=True, exist_ok=True)
            gen.generate_from_model(
                str(model_path),
                data_dir,
                templates_dir,
                output_dir,
                is_processing_batch=False,
                target_template=template_name,
            )

    return 0


def cmd_generate(args: Any, base_dir: Path) -> int:
    return _generate_single(
        Path(args.model).resolve(),
        args.profile,
        args.dry_run,
        base_dir,
    )


def cmd_regenerate(args: Any, base_dir: Path) -> int:
    from .manifest import get_component, all_components  # noqa: PLC0415

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
        models_dir = project_root / comp.models_dir
        if not models_dir.exists():
            log.warning(
                "Models directory not found for component %r: %s", comp_name, models_dir
            )
            continue

        globs = (
            (comp.entity_glob,)
            if isinstance(comp.entity_glob, str)
            else tuple(comp.entity_glob)
        )
        matches = set()
        for pattern in globs:
            matches.update(models_dir.glob(pattern))
        matches = {
            f for f in matches
            if comp.exclude_suffix is None or not f.name.endswith(comp.exclude_suffix)
        }

        # Second discovery root: the component's own modeling/ dir, if any.
        # Org files there are dispatched via #+type: rather than filename.
        if comp.modeling_dir:
            modeling_dir = project_root / comp.modeling_dir
            if modeling_dir.is_dir():
                for org_path in modeling_dir.glob("*.org"):
                    if _is_codegen_entity_org(org_path):
                        matches.add(org_path)

        model_files = sorted(matches)
        if not model_files:
            log.warning(
                "No %s files found in %s", comp.entity_glob, models_dir
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
            rc = _generate_single(model_path, args.profile, args.dry_run, base_dir)
            if rc != 0:
                total_errors += 1

    if total_errors:
        log.error("%d error(s) during regeneration.", total_errors)
    else:
        log.info("Regeneration complete.")

    return 1 if total_errors else 0
