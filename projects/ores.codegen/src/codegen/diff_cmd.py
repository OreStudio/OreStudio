import logging
from pathlib import Path
from typing import Any

log = logging.getLogger(__name__)


def cmd_diff(args: Any, base_dir: Path) -> int:
    log.error(
        "diff requires the unified sql_schema_create.mustache template, "
        "which has not yet been implemented. "
        "Complete task 'Implement unified SQL template' first."
    )
    return 1
