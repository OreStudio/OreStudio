#!/usr/bin/env python3
"""No two headers define a type of the same name in the same namespace.

Two definitions of one qualified name with different members never meet in
one translation unit, so the compiler does not see them. The linker does: the
templates instantiated for the type -- reflect-cpp's codecs among them -- have
one mangled name, the linker keeps one copy, and a message encoded with the
other type's layout reads the wrong members. Two
ores::iam::messaging::list_role_permissions_request types did this, and a
unity build's new object grouping turned the latent fault into a failed
encode.

The check reads every public header under projects/ and every test header,
takes each class or struct defined directly in a named namespace, and fails
when one qualified name is defined in two headers that can meet in one
binary: two public headers, a test header and a public header, or two test
headers of one test target. Test headers of different targets link into
different binaries, so they may share a name. A type in an anonymous
namespace has internal linkage and is check_unity_collisions.py's concern.

Usage:
  check_unique_type_names.py
"""

from __future__ import annotations

import collections
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
LEXEME = re.compile(
    r'R"(?P<delim>[^(\s]*)\(.*?\)(?P=delim)"|//[^\n]*|/\*.*?\*/|^\s*#[^\n]*'
    r'|"(?:[^"\\\n]|\\.)*"|\'(?:[^\'\\\n]|\\.)*\'', re.S | re.M)
TOKEN = re.compile(
    r"\bnamespace\s+(?P<namespace>[A-Za-z_][\w:]*)?\s*\{"
    r"|\b(?:struct|class)\s+(?:[A-Z][A-Z0-9_]+\s+)?(?P<type>[A-Za-z_]\w*)"
    r"(?:\s+final)?\s*(?::[^{;()]*)?\{"
    r"|\{|\}")
PUBLIC = "public"


def defined_types(text: str) -> set[str]:
    """The qualified name of each class or struct defined directly in a named
    namespace, at any nesting of named namespaces."""
    text = LEXEME.sub(" ", text)
    # Each open brace is a namespace name, None for an anonymous namespace,
    # or "" for any other scope: a type, a function or an initialiser.
    scopes: list[str | None] = []
    found = set()
    for m in TOKEN.finditer(text):
        token = m.group(0)
        if token == "}":
            if scopes:
                scopes.pop()
            continue
        in_namespaces = all(s for s in scopes)
        if m.group("type") is not None:
            if scopes and in_namespaces:
                found.add("::".join(scopes + [m.group("type")]))
            scopes.append("")
        elif token.startswith("namespace"):
            scopes.append(m.group("namespace"))
        else:
            scopes.append("")
    return found


def binary_of(header: pathlib.Path) -> str:
    """PUBLIC for a header any binary may include, else the test target whose
    sources include it."""
    parts = header.relative_to(ROOT).parts
    if "include" in parts:
        return PUBLIC
    return "/".join(parts[:parts.index("tests") + 1])


def clashes(defined: dict[str, dict[str, set[str]]]) -> dict[str, list[str]]:
    """Each qualified name defined twice where both definitions can meet in
    one binary, with the headers that define it."""
    out = {}
    for name, by_binary in defined.items():
        public = by_binary.get(PUBLIC, set())
        for binary, headers in by_binary.items():
            meeting = headers | public
            if len(meeting) > 1:
                out[name] = sorted(meeting)
                break
    return out


def headers() -> list[pathlib.Path]:
    public = ROOT.glob("projects/ores.*/**/include/**/*.hpp")
    tests = ROOT.glob("projects/ores.*/**/tests/**/*.hpp")
    return sorted({h for h in [*public, *tests] if "build" not in h.relative_to(ROOT).parts})


def main() -> int:
    defined: dict[str, dict[str, set[str]]] = collections.defaultdict(
        lambda: collections.defaultdict(set))
    for header in headers():
        for name in defined_types(header.read_text(errors="replace")):
            defined[name][binary_of(header)].add(str(header.relative_to(ROOT)))
    found = clashes(defined)
    for name, paths in sorted(found.items()):
        print(f"{name} is defined in {' and '.join(paths)}")
    if found:
        return 1
    print(f"ok: {len(defined)} type names, each defined once")
    return 0


if __name__ == "__main__":
    sys.exit(main())
