import json
import logging
from pathlib import Path
from typing import Any

from .core import (
    generate_from_model,
    get_model_type,
    load_model,
    load_profiles,
    resolve_output_path,
    resolve_profile_templates,
    validate_profile_for_model,
)

log = logging.getLogger(__name__)

_SAFE_PROFILES_FOR_ENTITY = frozenset({"sql"})

# Filter for org files in a component's modeling/ dir: only files whose
# frontmatter declares a codegen model type are picked up. Other org
# files (overviews, knowledge docs, plantuml source) are skipped.
from .manifest import is_codegen_entity_org as _is_codegen_entity_org  # noqa: E402


def _generate_single(
    model_path: Path,
    profile: str,
    dry_run: bool,
    base_dir: Path,
) -> int:
    if not model_path.exists():
        log.error("Model file not found: %s", model_path)
        return 1

    profiles = load_profiles(base_dir)
    model_type = get_model_type(model_path.name, model_path)

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

    is_valid, error = validate_profile_for_model(profile, profiles, model_type)
    if not is_valid:
        log.error("%s", error)
        return 1

    templates = resolve_profile_templates(profile, profiles, model_type)
    if not templates:
        log.error(
            "Profile %r has no applicable templates for model type %r",
            profile,
            model_type,
        )
        return 1

    model_data = load_model(model_path)

    project_root = base_dir.parent.parent
    data_dir = base_dir / "library" / "data"
    templates_dir = base_dir / "library" / "templates"

    for tmpl_info in templates:
        template_name = tmpl_info.get("template") if isinstance(tmpl_info, dict) else tmpl_info
        output_pattern = tmpl_info.get("output") if isinstance(tmpl_info, dict) else None

        if output_pattern:
            resolved = resolve_output_path(output_pattern, model_data, model_type)
            output_path = project_root / resolved
        else:
            output_path = None

        if dry_run:
            label = str(output_path) if output_path else f"<output>/{template_name}"
            print(label)
            continue

        if output_path:
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
        else:
            output_dir = base_dir / "output"
            output_dir.mkdir(parents=True, exist_ok=True)
            generate_from_model(
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
            rc = _generate_single(model_path, args.profile, args.dry_run, base_dir)
            if rc != 0:
                total_errors += 1

    if total_errors:
        log.error("%d error(s) during regeneration.", total_errors)
    else:
        log.info("Regeneration complete.")

    return 1 if total_errors else 0
