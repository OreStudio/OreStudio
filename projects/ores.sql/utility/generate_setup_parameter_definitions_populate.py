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
"""Write the seed of the Setup parameter definitions.

The seed holds one definition for each Setup parameter of the engine settings, with
the label, the help line, the full description, the default, the format and the ORE
releases that introduced and removed it. The texts are in setup_parameter_descriptions.py.
The analytic parameters have their own seed.

    generate_setup_parameter_definitions_populate.py          write the seed
    generate_setup_parameter_definitions_populate.py --check  fail if the seed no longer matches
"""

import argparse
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from setup_parameter_descriptions import SETUP_PARAMETERS  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                    "reporting_setup_parameter_definitions_populate.sql")

HEADER = """/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Generator: projects/ores.sql/utility/generate_setup_parameter_definitions_populate.py
 *
 * Setup Parameter Definitions Population Script
 *
 * Seeds one definition for each Setup parameter of the engine settings, with
 * the text a screen shows for it. This script is idempotent.
 */

\\echo '--- Setup Parameter Definitions ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
begin
"""

FOOTER = """end $$;
"""


def sql_text(value):
    if value is None:
        return "null"
    return "'" + value.replace("'", "''") + "'"


def render():
    parts = [HEADER]
    for position, p in enumerate(SETUP_PARAMETERS, start=1):
        parts.append(f"""    if not exists (
        select 1 from ores_reporting_parameter_definitions_tbl
        where tenant_id = v_sys_tenant and scope = 'Setup'
          and subtype = '' and name = {sql_text(p["name"])}
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_definitions_tbl (
            id, tenant_id, version,
            scope, subtype, name, position, parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            'Setup', '', {sql_text(p["name"])}, {position}, {sql_text(p["domain"])}, false,
            {sql_text(p["label"])}, {sql_text(p["help"])},
            {sql_text(p["description"])},
            {sql_text(p["default_text"])}, {sql_text(p["format"])},
            {sql_text(p["introduced_in"])}, {sql_text(p["removed_in"])},
            current_user, 'system.initial_load', {sql_text("Seed Setup parameter: " + p["name"])}
        );
    end if;

""")
    parts.append(FOOTER)
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
