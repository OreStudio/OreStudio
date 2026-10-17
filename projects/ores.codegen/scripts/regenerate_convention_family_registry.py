#!/usr/bin/env python3
"""Regenerate the convention family registry header.

Every refdata entity model that declares ``#+convention_family: <key>`` joins
the registry; see ``codegen.convention_family_registry``. The header is written
to a fixed path under the refdata API so the C++ service can iterate the
families instead of naming each one by hand.

Usage::

    regenerate_convention_family_registry.py            # write the header
    regenerate_convention_family_registry.py --check    # exit non-zero if stale
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects" / "ores.codegen" / "src"))

from codegen.convention_family_registry import (  # noqa: E402
    HEADER_RELATIVE_PATH,
    discover_entries,
    render_header,
)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--check",
        action="store_true",
        help="compare the committed header with a fresh render, writing nothing",
    )
    args = parser.parse_args()

    header = REPO_ROOT / HEADER_RELATIVE_PATH
    rendered = render_header(discover_entries(REPO_ROOT))

    if args.check:
        if not header.exists():
            print(f"missing {HEADER_RELATIVE_PATH}", file=sys.stderr)
            return 1
        if header.read_text(encoding="utf-8") != rendered:
            print(
                f"{HEADER_RELATIVE_PATH} is stale; run"
                " projects/ores.codegen/scripts/regenerate_convention_family_registry.py",
                file=sys.stderr,
            )
            return 1
        print(f"{HEADER_RELATIVE_PATH} is up to date")
        return 0

    header.parent.mkdir(parents=True, exist_ok=True)
    header.write_text(rendered, encoding="utf-8")
    print(f"wrote {HEADER_RELATIVE_PATH} ({len(rendered.splitlines())} lines)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
