import logging
from pathlib import Path
from typing import Any

log = logging.getLogger(__name__)


def cmd_list(args: Any, base_dir: Path) -> int:
    from .manifest import get_component, all_components, COMPONENTS  # noqa: PLC0415

    if args.what == "components":
        for name in all_components():
            comp = COMPONENTS[name]
            models_dir = base_dir.parent.parent / comp.models_dir
            if models_dir.exists():
                count = sum(
                    1 for f in models_dir.glob(comp.entity_glob)
                    if not f.name.endswith(comp.exclude_suffix)
                )
            else:
                count = 0
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

        models_dir = base_dir.parent.parent / comp.models_dir
        if not models_dir.exists():
            log.error("Models directory not found: %s", models_dir)
            return 1

        for path in sorted(
            f for f in models_dir.glob(comp.entity_glob)
            if not f.name.endswith(comp.exclude_suffix)
        ):
            print(path.name)
        return 0

    log.error("Unknown list target: %r", args.what)
    return 1
