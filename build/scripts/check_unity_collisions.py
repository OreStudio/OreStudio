#!/usr/bin/env python3
"""No two sources of a target define the same file-local name.

A unity build merges a target's sources into a few translation units, so two
files of one source directory that each define a name with internal linkage
-- in an anonymous namespace, static, or a const variable at namespace scope
-- in the same enclosing namespace redefine it, and the build fails. The
generator names every such helper after its entity, so generated code never
collides.

Hand-written code that broke the unity build when it was turned on is
recorded, not renamed in bulk: ``cmake/unity_exclusions.cmake`` keeps those
files out of the unity build. The record has two sources. This script's scan
finds names two files define, and keeps all but one file of each group out.
The compiler finds the rest -- types in anonymous namespaces, namespace
aliases, calls a merged unit makes ambiguous -- and ``--record`` reads them
from a failed unity build's log. A generated file is never recorded: its
template must be fixed.

The check fails when the scan finds a collision the record does not cover, or
when the record names a file that does not exist. Only a unity build proves a
recorded file no longer collides, so removing an entry is the paydown task's
job, checked by that build.

Usage:
  check_unity_collisions.py                   check the tree against the record
  check_unity_collisions.py --list            print every colliding name and its files
  check_unity_collisions.py --write           add the scan's collisions to the record
  check_unity_collisions.py --record <log>    add the files a unity build log blames
"""

from __future__ import annotations

import collections
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
EXCLUSIONS = ROOT / "cmake" / "unity_exclusions.cmake"
LEXEME = re.compile(
    r'R"\((?:.|\n)*?\)"|//[^\n]*|/\*.*?\*/|"(?:[^"\\\n]|\\.)*"|\'(?:[^\'\\\n]|\\.)*\'', re.S)
DECL = re.compile(
    r"^(?P<quals>(?:(?:inline|static|constexpr|const|thread_local)\s+)*)"
    r"(?:[\w:<>,\s\*&\[\]]+?)\s*\b(?P<name>[A-Za-z_]\w*)\s*(?:\(|=|\{|;)")
NOT_A_DECLARATION = ("#", "}", "using ", "template", "class ", "struct ", "enum ",
                     "namespace", "return", "typedef", "extern")
SKIPPED_PARTS = {"build", "node_modules"}


def blank_lexemes(text: str) -> str:
    """Blank out comments and literals, keeping line breaks."""
    return LEXEME.sub(lambda m: re.sub(r"[^\n]", " ", m.group(0)), text)


def internal_names(text: str) -> set[tuple[str, str]]:
    """The (enclosing named namespace, name) of each internal-linkage name
    defined at namespace scope."""
    stack: list[str | None] = []
    scope_depths: list[int] = []
    depth = 0
    found = set()
    for line in blank_lexemes(text).split("\n"):
        stripped = line.strip()
        at_namespace_scope = depth == (scope_depths[-1] if scope_depths else 0)
        if at_namespace_scope and stripped and not stripped.startswith(NOT_A_DECLARATION):
            m = DECL.match(stripped)
            if m:
                quals = m.group("quals").split()
                anonymous = any(n is None for n in stack)
                constant = ("const" in quals or "constexpr" in quals) and "inline" not in quals
                if anonymous or "static" in quals or constant:
                    found.add(("::".join(n for n in stack if n), m.group("name")))
        for m in re.finditer(r"\bnamespace\s+([A-Za-z_][\w:]*)?\s*\{|\{|\}", line):
            token = m.group(0)
            if token.startswith("namespace"):
                stack.append(m.group(1))
                depth += 1
                scope_depths.append(depth)
            elif token == "{":
                depth += 1
            else:
                if scope_depths and depth == scope_depths[-1]:
                    scope_depths.pop()
                    stack.pop()
                depth -= 1
    return found


def collisions() -> dict[tuple[str, str, str], list[str]]:
    """Each colliding (directory, namespace, name) and its files, sorted."""
    by_dir: dict[str, dict[tuple[str, str], list[str]]] = collections.defaultdict(
        lambda: collections.defaultdict(list))
    for cpp in sorted(ROOT.glob("projects/ores.*/**/*.cpp")):
        rel = cpp.relative_to(ROOT)
        if SKIPPED_PARTS & set(rel.parts):
            continue
        try:
            text = cpp.read_text()
        except UnicodeDecodeError:
            continue
        for namespace, name in internal_names(text):
            by_dir[str(rel.parent)][(namespace, name)].append(str(rel))
    return {(d, ns, name): files
            for d, names in by_dir.items()
            for (ns, name), files in names.items() if len(files) > 1}


def excluded(found: dict[tuple[str, str, str], list[str]]) -> set[str]:
    """The files to keep out of the unity build, so that at most one file of
    each colliding group stays in it."""
    out: set[str] = set()
    for _, files in sorted(found.items()):
        kept = [f for f in files if f not in out]
        out.update(kept[1:])
    return out


def target_dir(source: str) -> str:
    """The directory whose CMakeLists.txt builds the source."""
    path = (ROOT / source).parent
    while not (path / "CMakeLists.txt").exists():
        path = path.parent
    return str(path.relative_to(ROOT))


GENERATED = re.compile(r"AUTO-GENERATED FILE|Template: ")
SITE = re.compile(r"^(\S+?\.cpp):\d+:\d+: (error|note): (.*)$", re.M)
CULPRIT_NOTE = ("previous definition", "previous declaration", "candidate")


def is_generated(source: str) -> bool:
    return bool(GENERATED.search((ROOT / source).read_text()[:2000]))


def blamed(log: str) -> tuple[set[str], set[str]]:
    """The project sources a unity build log blames: those at an error, and
    those a note names as the other half of a clash. A note that only traces
    an instantiation names a victim, not a culprit, and is not blamed."""
    at_error, at_note = set(), set()
    for path, kind, message in SITE.findall(log):
        resolved = pathlib.Path(path)
        if not resolved.is_absolute():
            continue
        try:
            rel = resolved.resolve().relative_to(ROOT)
        except ValueError:
            continue
        if rel.parts[0] != "projects":
            continue
        if kind == "error":
            at_error.add(str(rel))
        elif message.startswith(CULPRIT_NOTE):
            at_note.add(str(rel))
    return at_error, at_note


def recorded() -> set[str]:
    if not EXCLUSIONS.exists():
        return set()
    return set(re.findall(r'"\$\{CMAKE_SOURCE_DIR\}/([^"]+\.cpp)"', EXCLUSIONS.read_text()))


def write(files: set[str]) -> None:
    by_target: dict[str, list[str]] = collections.defaultdict(list)
    for f in sorted(files):
        by_target[target_dir(f)].append(f)
    lines = [
        "# Hand-written sources that collide in a unity build, kept out of it.",
        "# Written by build/scripts/check_unity_collisions.py --write; the list",
        "# only shrinks. Fix a collision by naming the helper for what it does or",
        "# by sharing one definition, then remove the file here.",
    ]
    for target, sources in sorted(by_target.items()):
        lines.append("set_source_files_properties(")
        lines += [f'    "${{CMAKE_SOURCE_DIR}}/{s}"' for s in sources]
        lines.append(f'    DIRECTORY "${{CMAKE_SOURCE_DIR}}/{target}"')
        lines.append("    PROPERTIES SKIP_UNITY_BUILD_INCLUSION ON)")
    EXCLUSIONS.write_text("\n".join(lines) + "\n")


def main(argv: list[str]) -> int:
    found = collisions()
    if "--list" in argv:
        for (d, ns, name), files in sorted(found.items()):
            print(f"{d}  {ns or '(global)'}::{name}  {' '.join(pathlib.Path(f).name for f in files)}")
        return 0
    have = recorded()
    if "--write" in argv:
        merged = have | excluded({k: [f for f in fs if f not in have] for k, fs in found.items()})
        write(merged)
        print(f"recorded {len(merged)} files in {EXCLUSIONS.relative_to(ROOT)}")
        return 0
    if "--record" in argv:
        log = pathlib.Path(argv[argv.index("--record") + 1]).read_text(errors="replace")
        at_error, at_note = blamed(log)
        generated = sorted(f for f in at_error if is_generated(f))
        for f in generated:
            print(f"generated, fix its template: {f}")
        added = {f for f in at_error | at_note if not is_generated(f)} - have
        write(have | added)
        print(f"recorded {len(added)} more files, {len(have | added)} in all")
        return 1 if generated else 0
    missing = sorted(f for f in have if not (ROOT / f).exists())
    uncovered = []
    for (d, ns, name), files in sorted(found.items()):
        kept = [f for f in files if f not in have]
        if len(kept) > 1:
            uncovered.append(f"{ns or '(global)'}::{name} in {' '.join(kept)}")
    for u in uncovered:
        print(f"new collision: {u}")
    for f in missing:
        print(f"recorded file does not exist, remove it from {EXCLUSIONS.relative_to(ROOT)}: {f}")
    if uncovered or missing:
        return 1
    print(f"ok: {len(have)} recorded files, no collision outside them")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
