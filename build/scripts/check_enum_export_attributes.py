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

One pass over each file answers both questions, so the count that is reported
and the rule that is enforced agree: a site is a declaration when the name that
follows the enum keyword is followed by a colon, a brace or a semicolon, which
is how a declaration is spelled, and it is a violation when that name is a
visibility macro.

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
from typing import NamedTuple

SOURCE_SUFFIXES = (".hpp", ".h", ".cpp", ".ipp", ".cc")

DEFINE_RE = re.compile(r"^\s*#\s*define\s+([A-Za-z_]\w*)\s+(.+?)\s*$")
ATTRIBUTE_BODY_RE = re.compile(
    r"BOOST_SYMBOL_(?:EXPORT|IMPORT)"
    r"|__declspec\s*\(\s*dll(?:import|export)"
    r"|__attribute__\s*\(\(\s*visibility"
)

# The two names every component's export macro expands to. They are defined in
# Boost rather than under projects/, so a declaration that uses one directly
# would otherwise be invisible to the check.
BOOST_VISIBILITY_MACROS = {"BOOST_SYMBOL_EXPORT", "BOOST_SYMBOL_IMPORT"}

ENUM_KEYWORD_RE = re.compile(r"\benum\s+(?:(?:class|struct)\s+)?")
IDENTIFIER_RE = re.compile(r"[A-Za-z_]\w*")
# What follows an enum's name when it declares one, as opposed to naming an
# enum in an elaborated type or a using-declaration.
DECLARATION_TAIL_RE = re.compile(r"\s*(?::|\{|\;)")
USING_ENUM_RE = re.compile(r"\busing\s+$")


class EnumSite(NamedTuple):
    """One enum in one file, as the check reads it."""

    line: int
    source: str
    macro: str | None


def source_files(root: pathlib.Path) -> list[pathlib.Path]:
    return sorted(
        path
        for path in (root / "projects").rglob("*")
        if path.suffix in SOURCE_SUFFIXES and path.is_file()
    )


def erase_comments_and_literals(text: str) -> str:
    """Blank comments and string literals, keeping every offset in place.

    A one-to-one replacement matters twice over: line numbers are reported
    from these offsets, and a comment or a string that spells out the shape
    the check looks for must not be read as code. Blanks are substituted for
    the characters erased, newlines included, so nothing shifts.
    """
    out = list(text)
    index = 0
    length = len(text)
    while index < length:
        current = text[index]
        if current == "/" and text[index : index + 2] == "//":
            while index < length and text[index] != "\n":
                out[index] = " "
                index += 1
        elif current == "/" and text[index : index + 2] == "/*":
            while index < length and text[index : index + 2] != "*/":
                if text[index] != "\n":
                    out[index] = " "
                index += 1
            for offset in range(index, min(index + 2, length)):
                out[offset] = " "
            index += 2
        elif current in "\"'":
            quote = current
            out[index] = " "
            index += 1
            while index < length and text[index] != quote:
                if text[index] == "\\" and index + 1 < length:
                    out[index] = " "
                    index += 1
                if index < length and text[index] != "\n":
                    out[index] = " "
                index += 1
            if index < length:
                out[index] = " "
            index += 1
        else:
            index += 1
    return "".join(out)


def visibility_macros(sources: dict[pathlib.Path, str]) -> set[str]:
    """Every macro defined under projects/ whose body is a visibility attribute."""
    macros = set(BOOST_VISIBILITY_MACROS)
    for text in sources.values():
        joined = text.replace("\\\n", " ")
        for line in joined.splitlines():
            match = DEFINE_RE.match(line)
            if match and ATTRIBUTE_BODY_RE.search(match.group(2)):
                macros.add(match.group(1))
    return macros


def enum_sites(text: str, macros: set[str]) -> list[EnumSite]:
    """Every enum declaration in one comment- and literal-erased file."""
    sites: list[EnumSite] = []
    for keyword in ENUM_KEYWORD_RE.finditer(text):
        if USING_ENUM_RE.search(text, 0, keyword.start()):
            continue
        rest = text[keyword.end() :]
        first = IDENTIFIER_RE.match(rest)
        if first is None:
            continue
        macro = first.group(0) if first.group(0) in macros else None
        cursor = first.end()
        if macro is not None:
            skipped = len(rest[cursor:]) - len(rest[cursor:].lstrip())
            second = IDENTIFIER_RE.match(rest[cursor + skipped :])
            if second is None:
                continue
            cursor += skipped + second.end()
        if DECLARATION_TAIL_RE.match(rest[cursor:]) is None:
            continue
        line = text.count("\n", 0, keyword.start()) + 1
        sites.append(EnumSite(line, text.splitlines()[line - 1].strip(), macro))
    return sites


def read_sources(root: pathlib.Path) -> dict[pathlib.Path, str]:
    """Every C++ source under projects/, erased of comments and literals."""
    return {
        path: erase_comments_and_literals(path.read_text(errors="ignore"))
        for path in source_files(root)
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=".", help="repository root")
    args = parser.parse_args()

    root = pathlib.Path(args.root).resolve()
    if not (root / "projects").is_dir():
        print(f"error: {root}/projects is missing", file=sys.stderr)
        return 1

    sources = read_sources(root)
    macros = visibility_macros(sources)

    declarations = 0
    files = 0
    offending: list[tuple[str, EnumSite]] = []
    for path, text in sources.items():
        sites = enum_sites(text, macros)
        if sites:
            files += 1
            declarations += len(sites)
        offending.extend((str(path.relative_to(root)), site) for site in sites if site.macro)

    if not offending:
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
    for relative, site in offending:
        print(f"  {relative}:{site.line}: {site.source}", file=sys.stderr)
    print(
        "clang-cl rejects the combination with -Wignored-attributes, an error "
        "under -Werror. An enum needs no export attribute to cross a binary "
        "boundary; drop it.",
        file=sys.stderr,
    )
    return 1


if __name__ == "__main__":
    sys.exit(main())
