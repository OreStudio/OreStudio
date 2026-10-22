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
"""Write the seed of the analytic ore parameters.

The catalogue lists the parameters of the analytics in the order the run document
writes them. The text of each one is in the analytic_parameter_descriptions
package, and the release that introduced it is in analytic_parameter_releases.py.

    generate_analytic_ore_parameters_populate.py          write the seed
    generate_analytic_ore_parameters_populate.py --check  fail if the seed differs

Every parameter of the catalogue needs a complete text. The generator fails when one has none.
"""

import argparse
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from analytic_parameter_catalogue import CATALOGUE  # noqa: E402
from analytic_parameter_descriptions import DESCRIPTIONS  # noqa: E402
from analytic_parameter_releases import INTRODUCED_IN, REMOVED_IN  # noqa: E402
from ore_parameter_seed import SQL_FOOTER, TEXT_KEYS, header, insert_block  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                    "reporting_ore_parameters_populate.sql")


def rows():
    known = {(t, n) for t, n, _ in CATALOGUE}
    for analytic, parameters in DESCRIPTIONS.items():
        for name in parameters:
            if (analytic, name) not in known:
                raise ValueError(f"{analytic}.{name} has a text but is not in the catalogue")
    out = []
    for position, (analytic, name, domain) in enumerate(CATALOGUE, start=1):
        text = DESCRIPTIONS.get(analytic, {}).get(name, {})
        out.append({"scope": "analytic", "subtype": analytic, "name": name,
                    "position": position, "domain": domain,
                    "required": text.get("required", False),
                    "introduced_in": INTRODUCED_IN[f"{analytic}|{name}"],
                    "removed_in": REMOVED_IN.get(f"{analytic}|{name}"),
                    **{k: text.get(k) for k in TEXT_KEYS if k != "introduced_in"}})
    return out


def missing(all_rows):
    return [f"{r['subtype']}.{r['name']}" for r in all_rows
            if any(r.get(k) is None for k in TEXT_KEYS)]


def render(all_rows):
    parts = [header(
        "generate_analytic_ore_parameters_populate.py", "Analytic Ore Parameters",
        "Seeds the parameters of the analytics of ore.xml. The scope is analytic, the "
        "subtype is the analytic type and the name is the parameter. The text of "
        "each one is what a screen shows for it.")]
    for r in all_rows:
        parts.append(insert_block(r, f"Seed analytic parameter: {r['subtype']}.{r['name']}"))
    parts.append(SQL_FOOTER)
    return "".join(parts)


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--check", action="store_true",
                        help="fail if the committed seed differs from the texts")
    args = parser.parse_args()
    all_rows = rows()
    undocumented = missing(all_rows)
    if undocumented:
        print(f"{len(undocumented)} analytic parameters have no text, for example "
              f"{', '.join(undocumented[:5])}.", file=sys.stderr)
        return 1
    content = render(all_rows)
    if args.check:
        current = open(SEED).read() if os.path.exists(SEED) else ""
        if current != content:
            print(f"{SEED} no longer matches the texts; regenerate it.", file=sys.stderr)
            return 1
        print(f"{len(all_rows)} analytic parameters have text and match the seed.")
        return 0
    with open(SEED, "w") as f:
        f.write(content)
    print(f"Wrote {SEED}: {len(all_rows)} analytic parameters")
    return 0


if __name__ == "__main__":
    sys.exit(main())
