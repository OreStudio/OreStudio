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
"""Write the populate script of the engine settings groups.

The curated data lives in the seeder's curation file, one entry per group with
its settings in the order the screen draws them. This writes the two tables'
rows: the groups, and the settings of each group. The place of a group and of a
setting is the place it takes in the file, so the file is the order.

    generate_engine_settings_groups.py          write the populate script
    generate_engine_settings_groups.py --check  fail if the committed script differs
"""

import argparse
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import setup_value_sets  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
OUT = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                   "reporting_engine_settings_groups_populate.sql")

TENANT = "ores_utility_system_tenant_id_fn()"
ACTOR = "'ores_reporting_service', 'system.initial_load'"
SEED = "Seed engine settings groups"

LICENCE = """/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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
"""


def q(value):
    if value is None:
        return "null"
    if isinstance(value, bool):
        return "true" if value else "false"
    if isinstance(value, (int, float)):
        return str(value)
    return "'" + str(value).replace("'", "''") + "'"


def write(label, table, columns, rows):
    values = ",\n".join("    (" + ", ".join(r) + ")" for r in rows)
    return f"""\\echo '--- {label} ---'

insert into {table} (
    {columns}
) values
{values}
on conflict (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn()
do nothing;

select '{label}' as entity, count(*) as count
from {table}
where valid_to = ores_utility_infinity_timestamp_fn();
"""


def rows_of(data):
    groups, settings = [], []
    for group_position, group in enumerate(data["groups"]):
        groups.append([
            q(group["code"]), TENANT, "0", q(group["name"]), q(group["description"]),
            str(group_position), ACTOR, q(SEED)])
        for setting_position, setting in enumerate(group.get("settings", [])):
            kind, options, minimum, maximum = setup_value_sets.control_of(setting["value_set"])
            settings.append([
                q(f"{group['code']}:{setting['column']}"), TENANT, "0", q(group["code"]),
                q(setting["column"]), q(setting["ore"]), q(kind),
                q(setting.get("changes_numbers", False)),
                q(",".join(options)), q(minimum), q(maximum), str(setting_position),
                ACTOR, q(SEED)])
    return groups, settings


def validate(data):
    problems = []
    seen_groups, seen_settings = set(), set()
    for group in data["groups"]:
        code = group.get("code", "")
        if not code:
            problems.append("a group states no code")
        if code in seen_groups:
            problems.append(f"{code}: stated twice")
        seen_groups.add(code)
        for setting in group.get("settings", []):
            key = f"{code}:{setting.get('column', '')}"
            if key in seen_settings:
                problems.append(f"{key}: stated twice")
            seen_settings.add(key)
    return problems + setup_value_sets.problems()


def render(data):
    groups, settings = rows_of(data)
    return (
        f"""{LICENCE}
/**
 * Engine Settings Groups Population Script
 *
 * The groups of engine settings a configuration holds as parts, and the settings
 * of each group with the kind of value it takes. Generated by
 * projects/ores.sql/utility/generate_engine_settings_groups.py from the curation
 * file. Do not edit it by hand. This script is idempotent.
 */

"""
        + write("Engine Settings Groups", "ores_reporting_engine_settings_groups_tbl",
                "code, tenant_id, version, name, description, position, modified_by, "
                "change_reason_code, change_commentary", groups)
        + write("Engine Settings Group Settings",
                "ores_reporting_engine_settings_group_settings_tbl",
                "code, tenant_id, version, group_code, column_name, ore_name, kind, "
                "changes_numbers, options, minimum, maximum, position, modified_by, "
                "change_reason_code, change_commentary", settings))


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--check", action="store_true",
                        help="fail if the committed script differs from the data")
    args = parser.parse_args()
    data = setup_value_sets.groups()
    problems = validate(data)
    if problems:
        print("\n".join(problems), file=sys.stderr)
        return 1
    content = render(data)
    if args.check:
        current = open(OUT, encoding="utf-8").read() if os.path.exists(OUT) else ""
        if current != content:
            print(f"{OUT} differs from the curation; regenerate it.", file=sys.stderr)
            return 1
        print(f"{len(data['groups'])} groups match the script.")
        return 0
    with open(OUT, "w", encoding="utf-8") as handle:
        handle.write(content)
    print(f"Wrote {OUT}: {len(data['groups'])} groups")
    return 0


if __name__ == "__main__":
    sys.exit(main())
