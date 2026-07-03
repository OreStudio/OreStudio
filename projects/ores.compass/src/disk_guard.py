"""Disk-space guard shared by env provision and env upgrade.

Full (C++/vcpkg) environments consume tens of gigabytes once vcpkg and the
build tree are populated. Provisioning another worktree or promoting a light
one to full when the disk is nearly full leads to opaque mid-build failures,
so warn (and, when interactive, prompt) before committing to the operation.
"""

import shutil
import sys
from pathlib import Path

# Recommended minimum free space before provisioning/promoting a full env.
MIN_FREE_GB = 20


def check_disk_space(path: Path, *, assume_yes: bool = False,
                     min_free_gb: int = MIN_FREE_GB) -> bool:
    """Check free disk space at ``path``.

    Returns True if the operation should proceed, False to abort. When free
    space is at or above ``min_free_gb`` this is a silent pass. Below the
    threshold it warns; in an interactive session it prompts for confirmation
    (unless ``assume_yes``), and in a non-interactive session it proceeds after
    warning so automation is not blocked. Inability to read the free space is
    treated as a non-blocking warning.
    """
    try:
        usage = shutil.disk_usage(str(path))
    except OSError as e:
        print(f"⚠️  Could not determine free disk space at {path}: {e}",
              file=sys.stderr)
        return True

    free_gb = usage.free / (1024 ** 3)
    if free_gb >= min_free_gb:
        return True

    print(f"⚠️  Low disk space: {free_gb:.1f} GiB free at {path} "
          f"(recommended ≥ {min_free_gb} GiB for a full environment).",
          file=sys.stderr)

    if assume_yes:
        print("   Proceeding anyway (-y).", file=sys.stderr)
        return True
    if not sys.stdin.isatty():
        print("   Non-interactive session — proceeding.", file=sys.stderr)
        return True

    ans = input("   Continue anyway? [y/N] ").strip().lower()
    if ans != "y":
        print("Aborted.")
        return False
    return True
