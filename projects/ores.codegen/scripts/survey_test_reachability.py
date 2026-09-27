#!/usr/bin/env python3
"""Print a component's per-file test coverage survey as Markdown.

Item V08 of doc/knowledge/architecture/component_clean_standard.org asks
that every source file be exercised by a test or recorded with the reason
it is not, and says the list of files no test touches is the coverage work
item. This prints that list.

The measure is static include reachability. A source file counts as touched
when a test can reach it:

  1. Seed from every test source's includes.
  2. Follow each reachable header's own includes.
  3. When a header is reachable, follow the includes of the .cpp that
     implements it, because the translation unit the tests pull in at link
     time is part of what they exercise.
  4. A source file is touched when a header beside it, with the same stem
     and in the same directory, is reachable.

Read the result as reachability and not as coverage. A path that only runs
against a live server is invisible to it, and "touched" does not mean
"asserted" -- V07 is the item that asks whether the assertions are worth
anything.

The measure is not the same as the one the iam pass recorded, which also
considered symbol reachability, so the two sets of figures are not
comparable. Use this to find and group the work, and record which method
produced whatever number you write down.

The header index spans every component, so an include that crosses a
component boundary resolves.

Usage:
    python3 projects/ores.codegen/scripts/survey_test_reachability.py --component dq
    python3 projects/ores.codegen/scripts/survey_test_reachability.py --component dq --list
"""
from __future__ import annotations

import argparse
import re
import sys
from collections import defaultdict
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
PROJECTS = REPO_ROOT / "projects"

INCLUDE_RE = re.compile(r'^\s*#\s*include\s+"([^"]+)"')


def component_dirs(slug: str) -> list[Path]:
    """The component's parts: ores.<slug>/<part> plus the shell adapter."""
    dirs = []
    base = PROJECTS / f"ores.{slug}"
    if base.is_dir():
        dirs.extend(sorted(p for p in base.iterdir() if (p / "src").is_dir()))
    shell = PROJECTS / "ores.shell" / slug
    if shell.is_dir():
        dirs.append(shell)
    return dirs


def include_key(path: Path) -> str | None:
    """How a translation unit spells the include, e.g. ores.dq.core/a/b.hpp."""
    parts = path.parts
    if "include" not in parts:
        return None
    return "/".join(parts[parts.index("include") + 1:])


def header_index() -> dict[str, Path]:
    """Every header in the tree, keyed by the spelling an include uses."""
    index: dict[str, Path] = {}
    for h in PROJECTS.glob("*/*/include/**/*.hpp"):
        key = include_key(h)
        if key:
            index[key] = h
    return index


def edges_from(path: Path, index: dict[str, Path]) -> list[Path]:
    out = []
    try:
        text = path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return out
    for line in text.splitlines():
        m = INCLUDE_RE.match(line)
        if m and m.group(1) in index:
            out.append(index[m.group(1)])
    return out


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--component", required=True,
                    help="component slug, e.g. dq, iam, refdata")
    ap.add_argument("--list", action="store_true",
                    help="list every untouched file, grouped by directory")
    args = ap.parse_args()

    dirs = component_dirs(args.component)
    if not dirs:
        print(f"no component directories found for {args.component!r}", file=sys.stderr)
        return 2

    index = header_index()
    impl_for: dict[tuple[str, str], Path] = {}
    for d in dirs:
        for c in (d / "src").rglob("*.cpp"):
            impl_for[(c.parent.name, c.stem)] = c

    tests: list[Path] = []
    for d in dirs:
        t = d / "tests"
        if t.is_dir():
            tests.extend(sorted(t.rglob("*.cpp")))

    reachable: set[Path] = set()
    frontier: list[Path] = []
    for t in tests:
        frontier.extend(edges_from(t, index))
    while frontier:
        h = frontier.pop()
        if h in reachable:
            continue
        reachable.add(h)
        frontier.extend(edges_from(h, index))
        impl = impl_for.get((h.parent.name, h.stem))
        if impl is not None:
            frontier.extend(edges_from(impl, index))

    reachable_by_stem = {(h.parent.name, h.stem) for h in reachable}

    print(f"# Test-reachability survey: {args.component}")
    print()
    print(f"{len(index)} header(s) indexed; {len(tests)} test source(s); "
          f"{len(reachable)} header(s) reachable from them.")
    print()

    untouched: dict[str, list[str]] = defaultdict(list)
    rows: list[tuple[str, int, int]] = []
    for d in dirs:
        sources = sorted((d / "src").rglob("*.cpp"))
        hit = 0
        for s in sources:
            if (s.parent.name, s.stem) in reachable_by_stem:
                hit += 1
            else:
                rel = str(s.relative_to(REPO_ROOT))
                untouched[rel.split("/src/")[1].split("/")[0]].append(rel)
        rows.append((str(d.relative_to(REPO_ROOT)), hit, len(sources)))

    total_hit = sum(h for _, h, _ in rows)
    total = sum(n for _, _, n in rows)
    print("| Part | Touched | Sources |")
    print("|------+---------+---------|")
    for name, hit, n in rows:
        print(f"| ={name}= | {hit} | {n} |")
    print(f"| *total* | {total_hit} | {total} |")
    print()
    print(f"{total - total_hit} source file(s) no test reaches.")

    if args.list:
        print()
        for directory, files in sorted(untouched.items(), key=lambda kv: (-len(kv[1]), kv[0])):
            print(f"== {directory} ({len(files)})")
            for f in files:
                print(f"   {f}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
