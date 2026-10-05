#!/usr/bin/env python3
"""Static per-file survey of ores.marketdata src/ against its tests (V08).

A test reaches a source file when the test includes, directly or through other
marketdata files, the header whose stem matches the source file. Reaching a
header reaches its source file too, and that file's own includes are followed,
so an implementation detail a reached file uses is reached with it. A source file
that names its codec family in its stem (ore_key_fx.cpp) is reached with the
codec it implements. It is reach, not line coverage: a reached file may still
have paths no test runs.

Run from the repository root:

    python3 doc/agile/versions/v0/sprint_26/oresmd-handwritten-grammar/evidence/marketdata_test_reach.py
"""
import re
import sys
from collections import defaultdict
from pathlib import Path

ROOT = Path("projects/ores.marketdata")
PARTS = ("api", "client", "core", "service")
INCLUDE = re.compile(r'^\s*#\s*include\s+"([^"]+)"', re.M)


def headers():
    """Map every marketdata include path to its file."""
    found = {}
    for part in PARTS:
        inc = ROOT / part / "include"
        if inc.is_dir():
            for h in inc.rglob("*.hpp"):
                found[str(h.relative_to(inc))] = h
        src = ROOT / part / "src"
        if src.is_dir():
            for h in src.rglob("*.hpp"):
                found[str(h.relative_to(src))] = h
    return found


def sources():
    """Map every marketdata source stem to its files."""
    found = defaultdict(list)
    for part in PARTS:
        src = ROOT / part / "src"
        if src.is_dir():
            for c in src.rglob("*.cpp"):
                found[c.stem].append(c)
    return found


def owners(stem):
    """The stems whose header a source file implements: its own, and for a
    codec split across files, the codec's."""
    yield stem
    if stem.startswith("ore_key_") and stem != "ore_key_codec":
        yield "ore_key_codec"


def reached_headers(test, by_include, by_stem):
    seen, todo = set(), [test]
    while todo:
        f = todo.pop()
        if f.suffix == ".hpp":
            for c in by_stem.get(f.stem, ()):
                if c not in seen:
                    seen.add(c)
                    todo.append(c)
        for inc in INCLUDE.findall(f.read_text(errors="replace")):
            h = by_include.get(inc)
            if h is None:
                local = f.parent / inc
                h = local if local.is_file() else None
            if h is not None and h not in seen:
                seen.add(h)
                todo.append(h)
    return seen


def main():
    by_include = headers()
    by_stem = sources()
    stems_to_tests = defaultdict(set)
    for part in PARTS:
        tests = ROOT / part / "tests"
        if not tests.is_dir():
            continue
        for t in sorted(tests.glob("*.cpp")):
            if t.name == "main.cpp":
                continue
            for h in reached_headers(t, by_include, by_stem) | {t}:
                stems_to_tests[h.stem].add(f"{part}/{t.name}")

    rows = []
    for part in PARTS:
        src = ROOT / part / "src"
        if not src.is_dir():
            continue
        for s in sorted(src.rglob("*.cpp")):
            if s.name == "main.cpp":
                continue
            generated = "AUTO-GENERATED FILE" in s.read_text(errors="replace")[:2000]
            tests = sorted({t for o in owners(s.stem) for t in stems_to_tests.get(o, ())})
            rows.append((str(s.relative_to(ROOT)), generated, tests))

    reached = sum(1 for _, _, t in rows if t)
    print(f"{len(rows)} source files; {reached} reached by a test; {len(rows) - reached} not reached")
    print()
    for path, generated, tests in rows:
        kind = "generated" if generated else "hand-written"
        print(f"{path}\t{kind}\t{', '.join(tests) if tests else '-'}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
