#!/usr/bin/env python3
"""Regenerate the convention family registry header and apply fragment.

Every refdata entity model that declares ``#+convention_family: <key>`` joins
the registry; see ``codegen.convention_family_registry``. The header is written
to a fixed path under the refdata API, and the include fragment to the refdata
core service, so the C++ service compiles one call per family instead of
naming each one by hand.

Usage::

    regenerate_convention_family_registry.py            # write every artefact
    regenerate_convention_family_registry.py --check    # exit non-zero if stale
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(REPO_ROOT / "projects" / "ores.codegen" / "src"))

from codegen.convention_family_registry import (  # noqa: E402
    FRAGMENT_RELATIVE_PATH,
    HEADER_RELATIVE_PATH,
    INCLUDES_RELATIVE_PATH,
    discover_entries,
    render_fragment,
    render_header,
    render_includes,
)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--check",
        action="store_true",
        help="compare the committed artefacts with a fresh render, writing nothing",
    )
    args = parser.parse_args()

    entries = discover_entries(REPO_ROOT)
    artefacts = (
        (HEADER_RELATIVE_PATH, render_header(entries)),
        (FRAGMENT_RELATIVE_PATH, render_fragment(entries)),
        (INCLUDES_RELATIVE_PATH, render_includes(entries)),
    )

    if args.check:
        stale = False
        for relative_path, rendered in artefacts:
            path = REPO_ROOT / relative_path
            if not path.exists():
                print(f"missing {relative_path}", file=sys.stderr)
                stale = True
            elif path.read_text(encoding="utf-8") != rendered:
                print(
                    f"{relative_path} is stale; run"
                    " projects/ores.codegen/scripts/regenerate_convention_family_registry.py",
                    file=sys.stderr,
                )
                stale = True
        if stale:
            return 1
        for relative_path, _ in artefacts:
            print(f"{relative_path} is up to date")
        return 0

    for relative_path, rendered in artefacts:
        path = REPO_ROOT / relative_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(rendered, encoding="utf-8")
        print(f"wrote {relative_path} ({len(rendered.splitlines())} lines)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
