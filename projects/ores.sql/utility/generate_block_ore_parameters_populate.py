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
"""Write the seed of the Setup, Logging and Markets ore parameters.

The catalogue lists the parameters of the three blocks that are not engine settings. The text
of each one is in the block_parameter_descriptions package, and the release that introduced
it is in block_parameter_releases.py. The engine settings have their own seed.

    generate_block_ore_parameters_populate.py          write the seed
    generate_block_ore_parameters_populate.py --check  fail if the seed differs

Every parameter of the catalogue needs a complete text. The generator fails when one has none.
"""

import argparse
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from block_parameter_catalogue import BLOCK_CATALOGUE  # noqa: E402
from block_parameter_descriptions import DESCRIPTIONS  # noqa: E402
from block_parameter_releases import INTRODUCED_IN, REMOVED_IN  # noqa: E402
from ore_parameter_seed import SQL_FOOTER, TEXT_KEYS, header, insert_block  # noqa: E402
from setup_parameter_descriptions import SETUP_PARAMETERS  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                    "reporting_block_ore_parameters_populate.sql")


def rows():
    known = {(b, n) for b, n, _ in BLOCK_CATALOGUE}
    for block, parameters in DESCRIPTIONS.items():
        for name in parameters:
            if (block, name) not in known:
                raise ValueError(f"{block}.{name} has a text but is not in the catalogue")
    # The engine settings take the first positions of the Setup block.
    next_position = {"Setup": len(SETUP_PARAMETERS) + 1, "Logging": 1, "Markets": 1}
    out = []
    for block, name, domain in BLOCK_CATALOGUE:
        text = DESCRIPTIONS.get(block, {}).get(name, {})
        position = next_position[block]
        next_position[block] += 1
        out.append({"scope": block, "subtype": "", "name": name,
                    "position": position, "domain": domain,
                    "required": text.get("required", False),
                    "introduced_in": INTRODUCED_IN.get(f"{block}|{name}"),
                    "removed_in": REMOVED_IN.get(f"{block}|{name}"),
                    **{k: text.get(k) for k in TEXT_KEYS if k != "introduced_in"}})
    return out


def missing(all_rows):
    """The parameters with a text that is absent or blank, or a release that is not measured."""
    return [f"{r['scope']}.{r['name']}" for r in all_rows
            if any(not str(r.get(k) or "").strip() for k in TEXT_KEYS)]


def render(all_rows):
    parts = [header(
        "generate_block_ore_parameters_populate.py", "Block Ore Parameters",
        "Seeds the parameters of the Setup, Logging and Markets blocks of ore.xml that "
        "are not engine settings. The scope is the block and the name is the parameter. "
        "The text of each one is what a screen shows for it.")]
    for r in all_rows:
        parts.append(insert_block(r, f"Seed {r['scope']} parameter: {r['name']}"))
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
        print(f"{len(undocumented)} block parameters have no text, for example "
              f"{', '.join(undocumented[:5])}.", file=sys.stderr)
        return 1
    content = render(all_rows)
    if args.check:
        current = open(SEED).read() if os.path.exists(SEED) else ""
        if current != content:
            print(f"{SEED} no longer matches the texts; regenerate it.", file=sys.stderr)
            return 1
        print(f"{len(all_rows)} block parameters have text and match the seed.")
        return 0
    with open(SEED, "w") as f:
        f.write(content)
    print(f"Wrote {SEED}: {len(all_rows)} block parameters")
    return 0


if __name__ == "__main__":
    sys.exit(main())
