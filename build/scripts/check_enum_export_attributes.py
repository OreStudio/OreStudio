#!/usr/bin/env python3
"""Check that no visibility macro decorates an enum.

Why this exists
---------------

Every component's api header carries an export macro on each type that crosses
the binary boundary, and the macro is symmetrical enough to invite copying onto
an enum:

    enum class ORES_WORKFLOW_API_EXPORT failure_policy : std::uint8_t {

That is a defect. The macro expands to BOOST_SYMBOL_IMPORT or
BOOST_SYMBOL_EXPORT, which becomes __declspec(dllimport) or __declspec(dllexport)
on the Microsoft ABI, and neither attribute applies to an enum. clang-cl reports
it and -Werror turns the report into a failure:

    error: '__dllimport__' attribute only applies to functions, variables,
    classes, and Objective-C interfaces [-Werror,-Wignored-attributes]

It reached main on 2026-09-28 in ores.workflow and turned the Windows build red
at one translation unit of ores.dq.core.lib.

How it checks
-------------

It reads the macro definitions under projects/, keeps the ones whose body is a
symbol visibility or DLL storage attribute, and then looks for any of those
macros where an enum's name belongs -- directly after "enum", "enum class" or
"enum struct". Reading the definitions rather than matching an "EXPORT" suffix
keeps the rule honest: a name that looks like a macro but expands to something
else is not reported.

What it cannot see
------------------

This runs the same on every platform on purpose. Linux and macOS cannot catch
the defect at all: there BOOST_SYMBOL_IMPORT expands to
__attribute__((visibility("default"))), and clang and gcc accept a visibility
attribute on an enum in silence -- both were measured to compile the offending
declaration with -Wall -Wextra -Werror and no diagnostic. A static check is
therefore the only gate that fires before the Windows build does.

A visibility macro on an inapplicable target that is not an enum, and a DLL
attribute spelled out at the declaration instead of through a macro, still
reach CI.

Usage
-----

    python3 build/scripts/check_enum_export_attributes.py [--root .]
"""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

SOURCE_SUFFIXES = (".hpp", ".h", ".cpp", ".ipp", ".cc")

DEFINE_RE = re.compile(r"^\s*#\s*define\s+([A-Za-z_]\w*)\s+(.+?)\s*$")
ATTRIBUTE_BODY_RE = re.compile(
    r"BOOST_SYMBOL_(?:EXPORT|IMPORT)"
    r"|__declspec\s*\(\s*dll(?:import|export)"
    r"|__attribute__\s*\(\(\s*visibility"
)

# The macro is where the enum's name belongs, so it sits between the enum
# keyword and the name.
ENUM_RE = re.compile(r"\benum\s+(?:(?:class|struct)\s+)?([A-Za-z_]\w*)")

# The same shape, narrowed to a declaration, for the count that is reported on
# success: an elaborated type or a using-declaration names an enum without
# declaring one.
ENUM_DECLARATION_RE = re.compile(
    r"\benum\s+(?:(?:class|struct)\s+)?([A-Za-z_]\w*)\s*(?::|\{)"
)

LINE_COMMENT_RE = re.compile(r"//[^\n]*")
BLOCK_COMMENT_RE = re.compile(r"/\*.*?\*/", re.DOTALL)


def source_files(root: pathlib.Path) -> list[pathlib.Path]:
    return sorted(
        path
        for path in (root / "projects").rglob("*")
        if path.suffix in SOURCE_SUFFIXES and path.is_file()
    )


def visibility_macros(root: pathlib.Path) -> set[str]:
    macros: set[str] = set()
    for path in source_files(root):
        for line in path.read_text(errors="ignore").splitlines():
            match = DEFINE_RE.match(line)
            if match and ATTRIBUTE_BODY_RE.search(match.group(2)):
                macros.add(match.group(1))
    return macros


def strip_comments(text: str) -> str:
    """Blank out comments so prose about the rule is not read as the rule."""
    return BLOCK_COMMENT_RE.sub("", LINE_COMMENT_RE.sub("", text))


def offending_declarations(
    root: pathlib.Path, macros: set[str]
) -> list[tuple[str, int, str]]:
    found: list[tuple[str, int, str]] = []
    for path in source_files(root):
        lines = strip_comments(path.read_text(errors="ignore")).splitlines()
        for number, line in enumerate(lines, start=1):
            match = ENUM_RE.search(line)
            if match and match.group(1) in macros:
                found.append((str(path.relative_to(root)), number, line.strip()))
    return found


def enum_declaration_count(root: pathlib.Path) -> tuple[int, int]:
    declarations = 0
    files = 0
    for path in source_files(root):
        matches = ENUM_DECLARATION_RE.findall(
            strip_comments(path.read_text(errors="ignore"))
        )
        if matches:
            files += 1
            declarations += len(matches)
    return declarations, files


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=".", help="repository root")
    args = parser.parse_args()

    root = pathlib.Path(args.root).resolve()
    if not (root / "projects").is_dir():
        print(f"error: {root}/projects is missing", file=sys.stderr)
        return 1

    macros = visibility_macros(root)
    offending = offending_declarations(root, macros)

    if not offending:
        declarations, files = enum_declaration_count(root)
        print(
            f"Enum export attributes: none; {declarations} enum declaration(s) "
            f"in {files} file(s) carry no visibility macro."
        )
        return 0

    print(
        "A visibility macro decorates an enum, and the attribute does not apply "
        "to an enum:",
        file=sys.stderr,
    )
    for relative, number, line in offending:
        print(f"  {relative}:{number}: {line}", file=sys.stderr)
    print(
        "clang-cl rejects the combination with -Wignored-attributes, an error "
        "under -Werror. An enum needs no export attribute to cross a binary "
        "boundary; drop it.",
        file=sys.stderr,
    )
    return 1


if __name__ == "__main__":
    sys.exit(main())
