import logging
from pathlib import Path
from typing import Any

log = logging.getLogger(__name__)


def cmd_list(args: Any, base_dir: Path) -> int:
    from .manifest import (  # noqa: PLC0415
        COMPONENTS, all_components, discover_models, get_component,
    )

    project_root = base_dir.parent.parent

    if args.what == "components":
        for name in all_components():
            comp = COMPONENTS[name]
            count = len(discover_models(comp, project_root))
            print(f"{name:<20} {count} entity model(s)")
        return 0

    if args.what == "models":
        if not args.component:
            log.error("--component is required for 'list models'")
            return 1
        try:
            comp = get_component(args.component)
        except ValueError as exc:
            log.error("%s", exc)
            return 1

        models = discover_models(comp, project_root)
        if not models:
            log.error(
                "No models found for component %r (looked in %s and %s)",
                comp.name, comp.models_dir or "(no models_dir)",
                comp.modeling_dir or "(no modeling dir)",
            )
            return 1
        for path in models:
            print(path.name)
        return 0

    log.error("Unknown list target: %r", args.what)
    return 1
