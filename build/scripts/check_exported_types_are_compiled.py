#!/usr/bin/env python3
"""Fail when a library exports a type its own translation units never compile.

Why this exists
---------------

A component's ``export.hpp`` defines ``ORES_<COMPONENT>_<SUBLIB>_EXPORT`` as
``BOOST_SYMBOL_IMPORT`` for consumers and ``BOOST_SYMBOL_EXPORT`` while the
library itself is compiled. On the Microsoft ABI the import arm is
``__declspec(dllimport)``, so a consumer that constructs a type marked with it
references the symbol in the library's import library and cannot generate it
locally.

That is fine for a type the library defines, and fatal for a header-only type
the library never compiles: no translation unit ever sees the type with the
export arm, so the library exports nothing for it, and every consumer fails to
link:

    lld-link: error: undefined symbol: __declspec(dllimport) public:
      __cdecl ores::workflow::service::workflow_registry::workflow_registry(void)

The 2026-09-29 Windows clang-cl build died on exactly that, in
ores.workflow.api's two hand-written service headers, after the enum-export
fix let the build reach the link step for the first time.

Two ways out, and the check accepts either:

* drop the export attribute from a type that is header-only -- most of them
  are aggregates whose members are all defined in the header; or
* give the library a translation unit that includes the header, as
  ores.eventing.api does for =event_bus= in its =src/service/event_bus.cpp=,
  which is what makes the compiler emit and export the members.

Only Linux and macOS hide the defect, because there the import arm is a
visibility attribute that costs nothing. Windows is the only compiler that
reports it, and it reports it at link time, hours into a build.

Usage
-----

    python3 build/scripts/check_exported_types_are_compiled.py [--root .]
"""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

EXPORT_DEFINE_RE = re.compile(
    r"^\s*#\s*define\s+(ORES_[A-Z0-9_]+_EXPORT)\s+BOOST_SYMBOL", re.MULTILINE
)
LIBRARY_GUARD_RE = re.compile(r"#\s*ifdef\s+(ORES_[A-Z0-9_]+_LIBRARY)")
DECLARATION_RE = re.compile(
    r"^\s*(?:class|struct)\s+(ORES_[A-Z0-9_]+_EXPORT)\s+(\w+)", re.MULTILINE
)
INCLUDE_RE = re.compile(r'#\s*include\s+"([^"]+)"')


def include_roots(root: pathlib.Path) -> list[pathlib.Path]:
    return sorted((root / "projects").glob("*/*/include"))


def exported_types(header: pathlib.Path, macro: str) -> list[str]:
    text = header.read_text(errors="ignore")
    if "AUTO-GENERATED FILE" in text:
        return []
    return [m.group(2) for line in text.splitlines()
            if (m := DECLARATION_RE.match(line)) and m.group(1) == macro]


def reachable_headers(root: pathlib.Path, source_root: pathlib.Path) -> set[pathlib.Path]:
    """Every header the library's translation units include, directly or not."""
    roots = include_roots(root)
    seen: set[pathlib.Path] = set()
    queue = [path for path in source_root.rglob("*.cpp")] if source_root.is_dir() else []
    while queue:
        current = queue.pop()
        try:
            resolved = current.resolve()
        except OSError:
            continue
        if resolved in seen:
            continue
        seen.add(resolved)
        for name in INCLUDE_RE.findall(current.read_text(errors="ignore")):
            candidates = [current.parent / name] + [root / name for root in roots]
            for candidate in candidates:
                if candidate.is_file():
                    queue.append(candidate)
                    break
    return seen


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=".", help="repository root")
    args = parser.parse_args()
    root = pathlib.Path(args.root).resolve()

    offenders: list[tuple[str, str, list[str]]] = []
    checked = 0
    for export_header in sorted((root / "projects").glob("*/*/include/*/export.hpp")):
        text = export_header.read_text(errors="ignore")
        define = EXPORT_DEFINE_RE.search(text)
        guard = LIBRARY_GUARD_RE.search(text)
        if not define or not guard:
            continue
        macro = define.group(1)
        sublibrary = export_header.parents[2]
        source_root = sublibrary / "src"
        if not source_root.is_dir():
            continue
        reachable = reachable_headers(root, source_root)
        for header in sorted(sublibrary.joinpath("include").rglob("*.hpp")):
            types = exported_types(header, macro)
            if not types:
                continue
            checked += 1
            if header.resolve() not in reachable:
                offenders.append((str(header.relative_to(root)), macro, types))

    if not offenders:
        print(
            f"Exported types: {checked} hand-written header(s) export a type, and "
            "every one of them is compiled by its library."
        )
        return 0

    print(
        "A library exports a type that none of its translation units compiles, so "
        "the type is imported by consumers and defined by nobody:",
        file=sys.stderr,
    )
    for path, macro, types in offenders:
        print(f"  {path}: {', '.join(types)} ({macro})", file=sys.stderr)
    print(
        "On Windows that is an unresolved __declspec(dllimport) symbol when a "
        "consumer links. Either drop the attribute from the header-only type, or "
        "add a translation unit to the library that includes the header -- see "
        "ores.eventing.api's src/service/event_bus.cpp for the pattern.",
        file=sys.stderr,
    )
    return 1


if __name__ == "__main__":
    sys.exit(main())
