#!/usr/bin/env python3
# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""Measure the ORE release that introduced each analytic parameter.

An analytic parameter has two dates. The analytic type has a first release, and
the parameter name has a first release. The parameter cannot be older than the
analytic that holds it, so the later of the two is the answer.

A date comes from a checkout of the ORE sources. A name or type is present in a
release when a string literal in the C++ sources has it, or when an Analytic
block of an example, the user guide or a note has it. The first release with a
present name is its date.

The checkout has a tag for most releases. The release 1.8.12.0 has no tag, so
the script stands in the commit that bumped the version to 1.8.12.0. The tag of
each later release followed its bump by a few days.

The oldest tag is 1.8.0.7. A date equal to it only says "not later than 1.8.0.7".

Usage: measure_ore_parameter_releases.py --ore-source DIR [--json FILE]
"""

import argparse
import collections
import json
import re
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from analytic_parameter_catalogue import CATALOGUE  # noqa: E402

OLDEST = "1.8.0.7"
UNTAGGED = {"1.8.12.0": "8f30524c4"}

BLOCK = re.compile(r'<Analytic\s+type="([A-Za-z0-9_]+)"\s*>(.*?)</Analytic>', re.S)
PARAMETER = re.compile(r'<Parameter\s+name="([A-Za-z0-9_]+)"')
LITERAL = re.compile(r'"([A-Za-z][A-Za-z0-9_]*)"')


def git(source, *args):
    run = subprocess.run(("git", *args), cwd=source, capture_output=True,
                         text=True, errors="replace")
    return run.stdout


def version_key(version):
    return tuple(int(p) for p in version.split("."))


def releases(source):
    """Release version to the ref that holds it, oldest first."""
    refs = {t[1:]: t for t in git(source, "tag", "--list", "v*").split()}
    refs.update(UNTAGGED)
    return {v: refs[v] for v in sorted(refs, key=version_key)}


def analytic_pairs():
    return [(t, n) for t, n, _ in CATALOGUE]


def measure(source):
    pairs = analytic_pairs()
    wanted_pairs = set(pairs)
    types = {t for t, _ in pairs}
    names = {n for _, n in pairs}
    order = releases(source)
    first_type, first_name, first_pair = {}, {}, {}
    latest_names, latest_pairs = set(), set()
    for version, ref in order.items():
        latest_names, latest_pairs = set(), set()
        literals = set(LITERAL.findall(
            git(source, "grep", "-h", "-o", "-E", '"[A-Za-z][A-Za-z0-9_]*"',
                ref, "--", "*.cpp", "*.hpp")))
        for t in types & literals:
            first_type.setdefault(t, version)
        for n in names & literals:
            first_name.setdefault(n, version)
            latest_names.add(n)
        files = git(source, "grep", "-l", "-E", "Analytic type=", ref, "--").split("\n")
        for f in files:
            if not f.endswith((".xml", ".tex", ".md", ".txt")):
                continue
            for t, body in BLOCK.findall(git(source, "show", f)):
                if t in types:
                    first_type.setdefault(t, version)
                for n in PARAMETER.findall(body):
                    if (t, n) in wanted_pairs:
                        first_pair.setdefault((t, n), version)
                        latest_pairs.add((t, n))
                    if n in names:
                        first_name.setdefault(n, version)
                        latest_names.add(n)
    out = {}
    for t, n in pairs:
        candidates = [first_name.get(n), first_pair.get((t, n))]
        known = [c for c in candidates if c]
        type_date = first_type[t]
        name_date = min(known, key=version_key) if known else type_date
        out[f"{t}|{n}"] = max(name_date, type_date, key=version_key)
    gone = sorted(f"{t}|{n}" for t, n in pairs
                  if n not in latest_names and (t, n) not in latest_pairs)
    return out, order, gone


def display(version):
    return f"{version} or earlier" if version == OLDEST else version


def write_module(path, dates):
    licence = Path(__file__).read_text(encoding="utf-8").split('"""')[0].split("\n", 1)[1]
    lines = [licence.rstrip(),
             '"""The ORE release that introduced each analytic parameter.',
             "",
             "Written by measure_ore_parameter_releases.py. The key is the analytic type and",
             "the parameter name. The oldest tag of the checkout is 1.8.0.7, so a date equal",
             'to it reads "1.8.0.7 or earlier".',
             '"""',
             "",
             "INTRODUCED_IN = {"]
    for key in sorted(dates):
        lines.append(f'    "{key}": "{display(dates[key])}",')
    lines.append("}")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--ore-source", required=True, type=Path)
    parser.add_argument("--json", type=Path)
    parser.add_argument("--write-module", type=Path,
                        help="write the dates as the Python module the seed generator reads")
    args = parser.parse_args()
    out, order, gone = measure(args.ore_source)
    counts = collections.Counter(out.values())
    for version in order:
        if counts[version]:
            print(f"{display(version):>20}  {counts[version]}")
    print(f"{len(out)} analytic parameters")
    print(f"{len(gone)} absent from the latest release: {', '.join(gone)}")
    if args.write_module:
        write_module(args.write_module, out)
    if args.json:
        args.json.write_text(json.dumps(out, indent=1, sort_keys=True))
    return 0


if __name__ == "__main__":
    sys.exit(main())
