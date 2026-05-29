import json
import logging
import sys
from pathlib import Path
from typing import Any

log = logging.getLogger(__name__)

_SAFE_PROFILES_FOR_ENTITY = frozenset({"sql"})


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

    # Refuse --profile all for _entity.json (schema) models: running the all
    # profile silently overwrites production SQL with the wrong template when
    # both _entity.json and _domain_entity.json exist for the same entity.
    if profile == "all" and model_type == "schema":
        log.error(
            "--profile all is not allowed for _entity.json models. "
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

        models_dir = base_dir.parent.parent / comp.models_dir
        if not models_dir.exists():
            log.warning(
                "Models directory not found for component %r: %s", comp_name, models_dir
            )
            continue

        model_files = sorted(
            f for f in models_dir.glob(comp.entity_glob)
            if not f.name.endswith(comp.exclude_suffix)
        )
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
