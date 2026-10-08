#!/usr/bin/env python3
# -*- mode: python; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
"""
Derive the manual-pass edges for a component part's PlantUML class diagram.

Usage:
    manual_pass_edges.py --project ores.trading.api

generate_component_puml.py emits a box per class with its members and no edges.
This tool reads the same headers and prints the edges the boxes cannot show:

  * a data member whose type is a component-local type is ownership (*--);
  * a component-local type that appears only in a function signature is a
    dependency (..>);
  * a type from another component is left out.

The output is the manual section for the part's .puml, ready to paste below
the sentinel.
"""
from __future__ import annotations

import argparse
import re
import sys
from collections import defaultdict
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent / "scripts"))

from generate_component_puml import (  # noqa: E402
    TypeInfo,
    _find_include_dir,
    _project_dir,
    parse_header,
)

# A C++ identifier, which is how a type name appears inside a declaration.
_IDENTIFIER_RE = re.compile(r'[A-Za-z_]\w*')

COMMENT_BLOCK = """\
' Relationships the automated pass cannot see.
'
' The generator emits a box per class with its members and no edges, so every
' line below is read from the code rather than derived from it. A data member
' is ownership (*--); a type that appears only in a signature is a dependency
' (..>). Types from another component are left out, per the class-diagram
' conventions.
"""


def _qualified(namespace: tuple[str, ...], name: str) -> str:
    return "::".join((*namespace, name))


def _identifiers(type_str: str) -> set[str]:
    return set(_IDENTIFIER_RE.findall(type_str))


def collect_types(project_name: str) -> dict[str, TypeInfo]:
    """Map every component-local type's fully-qualified name to its TypeInfo."""
    include_dir = _find_include_dir(project_name)
    if include_dir is None:
        raise SystemExit(f"no include/ directory found for {project_name}")

    types: dict[str, TypeInfo] = {}
    for hpp in sorted(include_dir.rglob("*.hpp")):
        if hpp.name == f"{project_name}.hpp" or hpp.name == "export.hpp":
            continue
        for namespace, found in parse_header(hpp).items():
            for info in found:
                types[_qualified(namespace, info.name)] = info
    return types


def _short_names(types: dict[str, TypeInfo]) -> dict[str, list[str]]:
    short: dict[str, list[str]] = defaultdict(list)
    for qualified in types:
        short[qualified.rsplit("::", 1)[-1]].append(qualified)
    return short


def _resolve(namespace: tuple[str, ...], identifier: str,
             short: dict[str, list[str]]) -> str | None:
    """The component-local type an identifier names, or None.

    A bare name is resolved in the declaring type's own namespace first, then
    anywhere in the part when it is unambiguous. A qualified name is matched as
    a whole, so an external ores::utility::... type never resolves.
    """
    candidates = short.get(identifier)
    if not candidates:
        return None
    own = _qualified(namespace, identifier)
    if own in candidates:
        return own
    if len(candidates) == 1:
        return candidates[0]
    return None


def derive_edges(project_name: str) -> tuple[list[str], list[str], int, int]:
    types = collect_types(project_name)
    short = _short_names(types)

    ownership: set[tuple[str, str]] = set()
    dependencies: set[tuple[str, str]] = set()

    for owner, info in types.items():
        namespace = tuple(owner.split("::")[:-1])

        for member in info.members:
            for identifier in _identifiers(member.type_str):
                target = _resolve(namespace, identifier, short)
                if target and target != owner:
                    ownership.add((owner, target))

        for method in info.methods:
            for identifier in _identifiers(f"{method.type_str} {method.params}"):
                target = _resolve(namespace, identifier, short)
                if target and target != owner:
                    dependencies.add((owner, target))

    # A type that already owns the other does not also depend on it.
    dependencies -= ownership

    own_lines = [f"{a} *-- {b}" for a, b in ownership]
    dep_lines = [f"{a} ..> {b}" for a, b in dependencies]
    return sorted(own_lines + dep_lines), dep_lines, len(types), len(ownership)


def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--project", required=True, metavar="NAME",
                        help="component part to derive, e.g. ores.trading.api")
    args = parser.parse_args()

    edges, dep_lines, type_count, own_count = derive_edges(args.project)

    print(COMMENT_BLOCK)
    if edges:
        for line in edges:
            print(line)
    else:
        print("' No component-local edges: this part names only its own types and "
              "types from other components.")

    print(f"# {args.project}: {type_count} types, {own_count} ownership edges, "
          f"{len(dep_lines)} dependency edges", file=sys.stderr)


if __name__ == "__main__":
    main()
