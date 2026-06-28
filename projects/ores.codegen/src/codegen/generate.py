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
    compute_supported_set,
    compute_target_set,
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


def _generate_single(
    model_path: Path,
    profile: str,
    dry_run: bool,
    base_dir: Path,
) -> int:
    if not model_path.exists():
        log.error("Model file not found: %s", model_path)
        return 1

    templates_dir = base_dir / "library" / "templates"
    graph = load_graph(templates_dir)
    model_type = get_model_type(model_path.name, model_path)

    # Resolve which archetypes to generate by traversing the physical-space
    # graph: generation set = supported set (S_e) ∩ target set (T).
    if profile in _PROFILE_TO_FACETS:
        address = profile                       # legacy profile (compat shim)
        target = frozenset(_PROFILE_TO_FACETS[profile])
    else:
        address = profile                       # treated as a real address
        try:
            target = compute_target_set(address, graph)
        except ValueError as exc:
            log.error("%s", exc)
            return 1
    # TODO(B5): read the entity's :ores.*.enabled: drawer for per-entity
    # disables; empty props => S_e is every model-type-admissible facet.
    supported = compute_supported_set({}, graph, model_type)
    gen_facets = resolve_generation_set(supported, target)

    templates = []
    for facet in sorted(gen_facets):
        for arch in graph.facet_archetypes.get(facet, []):
            mts = arch.get("model_types")
            if mts and model_type not in mts:
                continue
            if not arch.get("template") or not arch.get("output"):
                continue
            templates.append({"template": arch["template"], "output": arch["output"]})

    if not templates:
        # Empty intersection is a warning, not an error (spec): nothing to do.
        log.warning("%s: nothing to generate for address %r (model type %r)",
                    model_path.name, address, model_type)
        return 0

    model_data = load_model(model_path)

    project_root = base_dir.parent.parent
    data_dir = base_dir / "library" / "data"

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
