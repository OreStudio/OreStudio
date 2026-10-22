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
"""Write the seed of the Setup ore parameters.

The seed holds one definition for each Setup parameter of the engine settings, with
the label, the help line, the full description, the default, the format and the ORE
releases that introduced and removed it. The texts are in setup_parameter_descriptions.py.
The analytic parameters have their own seed.

    generate_setup_ore_parameters_populate.py          write the seed
    generate_setup_ore_parameters_populate.py --check  fail if the seed no longer matches
"""

import argparse
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from ore_parameter_seed import SQL_FOOTER, header, insert_block  # noqa: E402
from setup_parameter_descriptions import SETUP_PARAMETERS  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                    "reporting_setup_ore_parameters_populate.sql")


def render():
    parts = [header(
        "generate_setup_ore_parameters_populate.py", "Setup Ore Parameters",
        "Seeds one definition for each Setup parameter of the engine settings, "
        "with the text a screen shows for it.")]
    for position, p in enumerate(SETUP_PARAMETERS, start=1):
        row = {**p, "scope": "Setup", "subtype": "", "position": position,
               "required": False}
        parts.append(insert_block(row, "Seed Setup parameter: " + p["name"]))
    parts.append(SQL_FOOTER)
    return "".join(parts)


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--check", action="store_true",
                        help="fail if the committed seed differs from the texts")
    args = parser.parse_args()
    content = render()
    if args.check:
        current = open(SEED).read() if os.path.exists(SEED) else ""
        if current != content:
            print(f"{SEED} no longer matches the texts; regenerate it.", file=sys.stderr)
            return 1
        print(f"{len(SETUP_PARAMETERS)} Setup parameters match the seed.")
        return 0
    with open(SEED, "w") as f:
        f.write(content)
    print(f"Wrote {SEED}: {len(SETUP_PARAMETERS)} Setup parameters")
    return 0


if __name__ == "__main__":
    sys.exit(main())
