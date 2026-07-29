#!/usr/bin/env python3
"""
Replace file(GLOB_RECURSE ...) source discovery in CMakeLists.txt with an
explicit, checked-in list of files.

Real CMake builds never re-run file(GLOB_RECURSE ...) unless CONFIGURE_DEPENDS
forces a reconfigure -- and even then, only Ninja/Makefiles notice a changed
file set, and only at the next build. Kitware's own guidance
(https://cmake.org/cmake/help/latest/command/file.html#glob-recurse) is that
CONFIGURE_DEPENDS is a workaround, not a substitute for an explicit list.
The idiomatic fix used across the CMake community is exactly this script:
keep the checked-in CMakeLists.txt free of GLOB entirely, and regenerate the
explicit list with a small external tool whenever files are added/removed.

Handles the two GLOB_RECURSE shapes used in this codebase:

  1. Plain source list (optionally preceded by `set(<var> "")`):
       file(GLOB_RECURSE files [CONFIGURE_DEPENDS] RELATIVE
           "${CMAKE_CURRENT_SOURCE_DIR}/"
           "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")
     -> set(files
            "path/one.cpp"
            "path/two.cpp"
        )

  2. Qt AUTOMOC header list (no RELATIVE, resolved via a component-root
     CMake variable, e.g. `set(ORES_QT_SYNTHETIC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/..)`):
       file(GLOB_RECURSE HEADERS [CONFIGURE_DEPENDS] "${ORES_QT_SYNTHETIC_DIR}/include/*.hpp")
     -> set(HEADERS
            "${ORES_QT_SYNTHETIC_DIR}/include/one.hpp"
        )
     (the variable prefix is kept, not resolved to an absolute path, so the
     list stays relocatable exactly like the original glob was)

Modes:
  (default)   Rewrite in place.
  --check     Exit non-zero if any file is stale (CI gate); nothing written.
"""
from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[3]
PROJECTS_DIR = REPO_ROOT / "projects"

# Matches an optional `set(<var> "")` immediately followed by the
# file(GLOB_RECURSE ...) call it seeds, or the call on its own.
GLOB_RE = re.compile(
    r"(?:set\(\s*(?P<setvar>\w+)\s+\"\"\s*\)\n)?"
    r"[ \t]*file\(GLOB_RECURSE\s+(?P<var>\w+)\s+"
    r"(?:CONFIGURE_DEPENDS\s+)?"
    r"(?:RELATIVE\s+\"(?P<relbase>[^\"]+)\"\s+)?"
    r"\"(?P<pattern>[^\"]+)\"\s*\)\n?",
    re.MULTILINE,
)

VAR_ASSIGN_TMPL = "set({var} {cmake_dir_expr}/..)"


def resolve_dir_var(text: str, var_expr: str, cmakelists_dir: Path) -> Path | None:
    """Resolve a ${VAR}[/subpath] directory expression to a real path.

    Only understands ${CMAKE_CURRENT_SOURCE_DIR} (this file's own directory)
    and component-root variables of the form
    `set(VAR ${CMAKE_CURRENT_SOURCE_DIR}/..)` found earlier in the same file
    -- the only two shapes this codebase's CMakeLists.txt files use.
    """
    m = re.match(r"\$\{(\w+)\}(?:/(.*))?$", var_expr)
    if not m:
        return None
    name, subpath = m.group(1), m.group(2)
    if name == "CMAKE_CURRENT_SOURCE_DIR":
        base = cmakelists_dir
    else:
        assign_re = re.compile(
            r"set\(\s*" + re.escape(name) + r"\s+\$\{CMAKE_CURRENT_SOURCE_DIR\}/\.\.\s*\)"
        )
        if not assign_re.search(text):
            return None
        base = cmakelists_dir.parent
    return (base / subpath) if subpath else base


def split_pattern(pattern: str) -> tuple[str, str]:
    """Split ".../*.ext" into (dir-expr-or-path, ".ext")."""
    dir_part, _, glob_part = pattern.rpartition("/")
    ext = "." + glob_part.split(".", 1)[1] if "." in glob_part else glob_part
    return dir_part, ext


def find_files(base: Path, ext: str) -> list[str]:
    return sorted(
        str(p.relative_to(base)).replace("\\", "/")
        for p in base.rglob(f"*{ext}")
        if p.is_file()
    )


def render_block(var: str, items: list[str], prefix: str | None) -> str:
    if not items:
        return f'set({var} "")\n'
    lines = [f"set({var}"]
    for it in items:
        path = f"{prefix}/{it}" if prefix else it
        lines.append(f'    "{path}"')
    lines.append(")")
    return "\n".join(lines) + "\n"


def process_file(path: Path, check: bool) -> bool:
    """Returns True if the file is stale (would change / did change)."""
    text = path.read_text(encoding="utf-8")
    cmakelists_dir = path.parent
    changed = False

    def repl(m: re.Match) -> str:
        nonlocal changed
        var = m.group("var")
        relbase = m.group("relbase")
        pattern = m.group("pattern")
        dir_expr, ext = split_pattern(pattern)

        if relbase is not None:
            base_dir = resolve_dir_var(text, relbase.rstrip("/"), cmakelists_dir)
            if base_dir is None:
                print(f"  [skip] unresolvable RELATIVE base in {path}: {relbase}",
                      file=sys.stderr)
                return m.group(0)
            items = find_files(base_dir, ext)
            block = render_block(var, items, prefix=None)
        else:
            base_dir = resolve_dir_var(text, dir_expr, cmakelists_dir)
            if base_dir is None:
                print(f"  [skip] unresolvable dir expr in {path}: {dir_expr}",
                      file=sys.stderr)
                return m.group(0)
            items = find_files(base_dir, ext)
            block = render_block(var, items, prefix=dir_expr)

        changed = True
        return block

    new_text = GLOB_RE.sub(repl, text)
    if not changed:
        return False
    if new_text == text:
        return False
    if not check:
        path.write_text(new_text, encoding="utf-8")
    return True


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                  formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("paths", nargs="*",
                    help="CMakeLists.txt files or directories to process "
                         "(default: every CMakeLists.txt under projects/).")
    ap.add_argument("--check", action="store_true",
                    help="Exit non-zero if any file is stale; write nothing.")
    args = ap.parse_args(argv)

    if args.paths:
        targets: list[Path] = []
        for p in args.paths:
            p = Path(p).resolve()
            if p.is_dir():
                targets.extend(sorted(p.rglob("CMakeLists.txt")))
            else:
                targets.append(p)
    else:
        targets = sorted(PROJECTS_DIR.rglob("CMakeLists.txt"))

    stale = []
    for t in targets:
        if process_file(t, args.check):
            stale.append(t)
            verb = "stale" if args.check else "updated"
            print(f"{verb}: {t.relative_to(REPO_ROOT)}")

    if args.check and stale:
        print(f"\n{len(stale)} file(s) would change. Run without --check to fix.",
              file=sys.stderr)
        return 1
    if not stale:
        print("All CMakeLists.txt source lists up to date.")
    else:
        print(f"\n{len(stale)} file(s) updated.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
