#!/usr/bin/env python3
"""
One-off migration helper: replace the inline `set(files ...)` /
`set(HEADERS ...)` blocks left by the earlier GLOB_RECURSE migration
pass with `include(${CMAKE_CURRENT_SOURCE_DIR}/component_files.cmake)`,
for every component that now has a generated component_files.cmake
(see regenerate_cmake_component_files.py).

Not meant to be re-run routinely -- once a CMakeLists.txt has the
include() line, there is nothing left for this script to do there.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]

_INCLUDE_LINE = "include(${CMAKE_CURRENT_SOURCE_DIR}/component_files.cmake)\n"

# Matches the inline set(files ...) block, optionally followed by a
# blank line and a set(HEADERS ...) block (the Qt AUTOMOC case) and its
# preceding comment line.
_BLOCK_RE = re.compile(
    r"set\(files\n(?:.*\n)*?\)\n"
    r"(?:\n(?:# Headers must be listed for AUTOMOC[^\n]*\n)?set\(HEADERS\n(?:.*\n)*?\)\n)?",
)


def main() -> int:
    targets = sorted(REPO_ROOT.glob("projects/**/component_files.cmake"))
    dirs = {p.parent for p in targets}
    changed = []
    for d in sorted(dirs):
        cmakelists = d / "CMakeLists.txt"
        if not cmakelists.exists():
            continue
        text = cmakelists.read_text(encoding="utf-8")
        new_text, n = _BLOCK_RE.subn(_INCLUDE_LINE, text, count=1)
        if n and new_text != text:
            cmakelists.write_text(new_text, encoding="utf-8")
            changed.append(cmakelists)
            print(f"wired: {cmakelists.relative_to(REPO_ROOT)}")

    print(f"\n{len(changed)} CMakeLists.txt wired to include component_files.cmake.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
