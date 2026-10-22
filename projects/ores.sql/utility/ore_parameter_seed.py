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
"""Shared pieces of the generators that write the ore parameter seeds.

A seed row is a dict with the keys scope, subtype, name, position, domain,
required, the text keys label, help, description, default_text, format and
introduced_in, and removed_in. The schema name and the XPath follow from the
scope, subtype and name.
"""

import textwrap

SCHEMA_NAME = "ore.xsd"

TEXT_KEYS = ("label", "help", "description", "default_text", "format", "introduced_in")

SQL_HEADER = """/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Generator: projects/ores.sql/utility/{generator}
 *
 * {title} Population Script
 *
 * {summary}
 */

\\echo '--- {title} ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
begin
"""

SQL_FOOTER = """end $$;
"""


def header(generator, title, summary):
    """The licence and the opening of the seed, with the summary wrapped."""
    lines = textwrap.wrap(summary + " This script is idempotent.", width=76)
    return SQL_HEADER.format(generator=generator, title=title,
                             summary="\n * ".join(lines))


def sql_text(value):
    if value is None:
        return "null"
    return "'" + value.replace("'", "''") + "'"


def xpath(row):
    if row["scope"] == "Setup":
        return f"/ORE/Setup/Parameter[@name='{row['name']}']"
    return (f"/ORE/Analytics/Analytic[@type='{row['subtype']}']"
            f"/Parameter[@name='{row['name']}']")


def insert_block(row, comment):
    """The idempotent insert of one ore parameter."""
    text = [sql_text(row.get(k)) for k in TEXT_KEYS] + [sql_text(row.get("removed_in"))]
    required = "true" if row.get("required") else "false"
    return f"""    if not exists (
        select 1 from ores_reporting_ore_parameters_tbl
        where tenant_id = v_sys_tenant and scope = {sql_text(row["scope"])}
          and subtype = {sql_text(row["subtype"])} and name = {sql_text(row["name"])}
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_ore_parameters_tbl (
            id, tenant_id, version,
            schema_name, xpath, scope, subtype, name, position, ore_parameter_value_domain_code, is_required,
            label, help, description, default_text, format, introduced_in, removed_in,
            modified_by, change_reason_code, change_commentary
        ) values (
            gen_random_uuid(), v_sys_tenant, 0,
            {sql_text(SCHEMA_NAME)}, {sql_text(xpath(row))},
            {sql_text(row["scope"])}, {sql_text(row["subtype"])}, {sql_text(row["name"])}, {row["position"]}, {sql_text(row["domain"])}, {required},
            {text[0]}, {text[1]},
            {text[2]},
            {text[3]}, {text[4]},
            {text[5]}, {text[6]},
            current_user, 'system.initial_load', {sql_text(comment)}
        );
    end if;

"""
