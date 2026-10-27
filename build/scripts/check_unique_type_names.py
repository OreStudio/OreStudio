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

The check reads every public header under projects/, takes each class or
struct defined at the top of its outermost namespace, and fails when one
qualified name is defined in two headers.

Usage:
  check_unique_type_names.py
"""

from __future__ import annotations

import collections
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
COMMENT = re.compile(r"//[^\n]*|/\*.*?\*/", re.S)
NAMESPACE = re.compile(r"^namespace\s+([\w:]+)\s*\{", re.M)
DEFINITION = re.compile(
    r"^(?:struct|class)\s+(?:[A-Z][A-Z0-9_]+\s+)?([a-z_]\w*)(?:\s+final)?\s*(?::[^{;]*)?\{", re.M)


def main() -> int:
    defined: dict[tuple[str, str], set[str]] = collections.defaultdict(set)
    for header in sorted(ROOT.glob("projects/ores.*/**/include/**/*.hpp")):
        text = COMMENT.sub("", header.read_text(errors="replace"))
        namespaces = NAMESPACE.findall(text)
        if not namespaces:
            continue
        for name in DEFINITION.findall(text):
            defined[(namespaces[0], name)].add(str(header.relative_to(ROOT)))
    clashes = {k: v for k, v in defined.items() if len(v) > 1}
    for (namespace, name), headers in sorted(clashes.items()):
        print(f"{namespace}::{name} is defined in {' and '.join(sorted(headers))}")
    if clashes:
        return 1
    print(f"ok: {len(defined)} type names, each defined once")
    return 0


if __name__ == "__main__":
    sys.exit(main())
