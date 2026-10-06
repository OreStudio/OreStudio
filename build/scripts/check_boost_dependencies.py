#!/usr/bin/env python3
"""Check that every Boost header the tree includes has a vcpkg port behind it.

Why this exists
---------------

Boost is installed through vcpkg as per-library ports, not as one package. A
new library therefore compiles for whoever added it -- their vcpkg tree was
installed before the include was written, or they installed the port by hand --
and fails on a clean vcpkg install in CI with

    fatal error: boost/multiprecision/cpp_dec_float.hpp: No such file or directory

That is how boost-multiprecision reached main on 2026-09-28 and turned every
continuous build red at one translation unit. The build is the gate, but it is
a slow, late one: the failure appears after vcpkg and twenty minutes of
compiling, on main rather than on the pull request that caused it.

How it checks
-------------

It reads the ports' own manifests from the vendored vcpkg checkout, takes the
transitive closure of the declared dependencies, and compares that with the
boost/<library>/ headers the tree actually includes. A library that arrives
transitively is accepted, which is why boost-math does not need its own line:
it comes with boost-multiprecision.

Most Boost headers sit under a directory named after their vcpkg port. The
exceptions are listed in PORT_FOR_INCLUDE: Boost.ContainerHash ships the
deprecated boost/functional/hash* headers, so they belong to
boost-container-hash even though the rest of boost/functional belongs to
boost-functional.

What it cannot see
------------------

A header whose port is not named after its directory and not listed in
PORT_FOR_INCLUDE, and any third-party library that is not Boost -- sqlgen,
reflectcpp or immer landing without a manifest entry would still reach CI.

Usage
-----

    python3 build/scripts/check_boost_dependencies.py [--root .]
"""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys

# Directories under boost/ that are not libraries, so no port provides them.
# boost/pending holds pieces the graph library and others include from
# themselves; it has no vcpkg port because it is not installable on its own.
NOT_LIBRARIES = {"pending"}

# Headers whose vcpkg port is not named after the directory they live in.
# Boost.ContainerHash ships the deprecated boost/functional/hash* headers, so
# that path belongs to boost-container-hash even though the rest of
# boost/functional belongs to boost-functional. Keyed by the include path
# after "boost/" with the extension removed; a key covers its exact path and
# anything under it.
PORT_FOR_INCLUDE = {
    "functional/hash": "boost-container-hash",
    "functional/hash_fwd": "boost-container-hash",
}

INCLUDE_RE = re.compile(r'#\s*include\s*[<"]boost/([a-z0-9_]+/[a-z0-9_/.]*)[>"]')
SOURCE_SUFFIXES = (".hpp", ".cpp")


def vcpkg_root(root: pathlib.Path) -> pathlib.Path:
    return root / "vcpkg"


def declared_names(root: pathlib.Path) -> set[str]:
    manifest = json.loads((root / "vcpkg.json").read_text())
    names = set()
    for dependency in manifest.get("dependencies", []):
        names.add(dependency if isinstance(dependency, str) else dependency["name"])
    return names


def dependencies_of(ports: pathlib.Path, port: str) -> list[str]:
    manifest = ports / port / "vcpkg.json"
    if not manifest.exists():
        return []
    try:
        parsed = json.loads(manifest.read_text())
    except json.JSONDecodeError:
        return []
    out = []
    for dependency in parsed.get("dependencies", []):
        out.append(dependency if isinstance(dependency, str) else dependency["name"])
    return out


def installed_closure(ports: pathlib.Path, declared: set[str]) -> set[str]:
    closure: set[str] = set()
    stack = list(declared)
    while stack:
        port = stack.pop()
        if port in closure:
            continue
        closure.add(port)
        stack.extend(dependencies_of(ports, port))
    return closure


def boost_headers_used(root: pathlib.Path) -> dict[str, tuple[str, str]]:
    """Every boost/<header> included under projects/, keyed by its vcpkg port,
    with the header and one file that includes it."""
    used: dict[str, tuple[str, str]] = {}
    for path in (root / "projects").rglob("*"):
        if path.suffix not in SOURCE_SUFFIXES or not path.is_file():
            continue
        for match in INCLUDE_RE.finditer(path.read_text(errors="ignore")):
            include = match.group(1)
            if include.split("/")[0] in NOT_LIBRARIES:
                continue
            used.setdefault(port_for(include),
                            (include, str(path.relative_to(root))))
    return used


def port_for(include: str) -> str:
    """The vcpkg port that installs boost/<include>."""
    stem = include.rsplit(".", 1)[0]
    for prefix, port in PORT_FOR_INCLUDE.items():
        if stem == prefix or stem.startswith(prefix + "/"):
            return port
    return "boost-" + include.split("/")[0].replace("_", "-")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", default=".", help="repository root")
    args = parser.parse_args()

    root = pathlib.Path(args.root).resolve()
    ports = vcpkg_root(root) / "ports"
    if not ports.is_dir():
        print(
            f"error: {ports} is missing; check out the vcpkg submodule "
            "(git submodule update --init vcpkg)",
            file=sys.stderr,
        )
        return 1

    declared = declared_names(root)
    closure = installed_closure(ports, declared)
    used = boost_headers_used(root)

    missing = {port: entry for port, entry in sorted(used.items())
               if port not in closure}

    if not missing:
        print(
            f"Boost dependencies declared: every boost/ header the tree includes "
            f"is provided by the {len(closure)} port(s) vcpkg installs."
        )
        return 0

    print("Boost headers are included but their vcpkg port is not installed:", file=sys.stderr)
    for port, (include, first_use) in missing.items():
        print(f"  boost/{include} ({first_use}) -> add \"{port}\" to vcpkg.json",
              file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
