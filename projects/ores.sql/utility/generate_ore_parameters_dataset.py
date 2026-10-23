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
"""Write the DQ datasets of the ore parameters and their value domains.

The dataset ore.parameter_value_domains holds the kinds of value a parameter can have. The
dataset ore.parameters holds the documentation of every leaf of ore.xml that a screen shows:
the engine settings of the Setup block, the other parameters of the Setup, Logging and Markets
blocks, and the parameters of the analytics. The rows come from the row modules and the texts
they read. Both datasets are members of the system_core and base bundles, and a publication
writes them to the tenant that the bundle names.

    generate_ore_parameters_dataset.py          write the seed
    generate_ore_parameters_dataset.py --check  fail if the seed differs

Every parameter needs a complete text. The generator fails when one has none.

The XPath of a leaf is its identity. The id of a parameter derives from it, so a leaf that
is renamed or moved is a new parameter in every tenant, and the old row stays until a
replace-all publish closes it.
"""

import argparse
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import analytic_parameter_rows  # noqa: E402
import block_parameter_rows  # noqa: E402
import setup_parameter_rows  # noqa: E402
from ore_parameter_seed import artefact_row, parameter_id, sql_text, xpath  # noqa: E402

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
SEED = os.path.join(ROOT, "projects", "ores.sql", "populate", "reporting",
                    "reporting_ore_parameters_artefact_populate.sql")

VALUE_DOMAINS = (
    ("string", "Text", "string"),
    ("boolean", "Boolean", "boolean"),
    ("integer", "Integer", "integer"),
    ("double", "Decimal", "double"),
    ("date", "Date", "date"),
)

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
 * Generator: projects/ores.sql/utility/generate_ore_parameters_dataset.py
 *
 * Ore Parameters DQ Datasets
 *
 * Registers the datasets ore.parameter_value_domains and ore.parameters and
 * stages their rows. Both are members of the system_core and base bundles, so
 * a publication writes them into the system tenant and into each tenant, through
 * ores_reporting_publish_ore_parameter_value_domains_from_dq_fn and
 * ores_reporting_publish_ore_parameters_from_dq_fn. This script is idempotent:
 * it replaces the artefact rows of each dataset.
 */

"""

DATASET = """\\echo '--- DQ Dataset: {title} ---'

do $$
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        '{code}',
        'ORE',
        'General',
        'Reference Data',
        'NONE',
        'Primary',
        'Actual',
        'Raw',
        'ORE Internal',
        '{title}',
        {description},
        'ORE',
        {context},
        '2026-10-23'::date,
        'Modified BSD License',
        '{artefact_type}'
    );
end $$;

\\echo '--- DQ Artefact: {title} ---'

do $$
declare
    v_dataset_id uuid;
begin
    select id into v_dataset_id
    from ores_dq_datasets_tbl
    where code = '{code}'
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_dataset_id is null then
        raise exception 'Dataset {code} not found.';
    end if;

    delete from {table}
    where dataset_id = v_dataset_id;

    insert into {table} (
        {columns}
    ) values
{rows};
end $$;

"""

DOMAIN_COLUMNS = "dataset_id, tenant_id, code, version, name, storage_kind, referenced_entity"

PARAMETER_COLUMNS = (
    "dataset_id, tenant_id, id, version, scope, subtype, name, schema_name, xpath, position,\n"
    "        ore_parameter_value_domain_code, is_required, label, help, description, default_text,\n"
    "        format, introduced_in, removed_in")


def rows():
    out = (setup_parameter_rows.rows() + block_parameter_rows.rows()
           + analytic_parameter_rows.rows())
    ids = [parameter_id(r) for r in out]
    if len(set(ids)) != len(ids):
        raise ValueError("two parameters document the same leaf")
    unknown = {r["domain"] for r in out} - {code for code, _, _ in VALUE_DOMAINS}
    if unknown:
        raise ValueError(f"no value domain for {sorted(unknown)}")
    return out


def missing():
    return (setup_parameter_rows.missing(setup_parameter_rows.rows())
            + block_parameter_rows.missing(block_parameter_rows.rows())
            + analytic_parameter_rows.missing(analytic_parameter_rows.rows()))


def render(all_rows):
    domain_rows = ",\n".join(
        f"        (v_dataset_id, ores_utility_system_tenant_id_fn(), {sql_text(code)}, 0, "
        f"{sql_text(name)}, {sql_text(kind)}, '')"
        for code, name, kind in VALUE_DOMAINS)
    parameter_rows = ",\n".join(artefact_row(r) for r in all_rows)
    return (SQL_HEADER
            + DATASET.format(
                title="Ore Parameter Value Domains", code="ore.parameter_value_domains",
                description=sql_text("The kinds of value an ORE parameter can have: text, "
                                     "boolean, integer, decimal and date."),
                context=sql_text("Value kinds for the ORE parameter documentation"),
                artefact_type="ore_parameter_value_domains",
                table="ores_dq_ore_parameter_value_domains_artefact_tbl",
                columns=DOMAIN_COLUMNS, rows=domain_rows)
            + DATASET.format(
                title="Ore Parameters", code="ore.parameters",
                description=sql_text("The documentation of the leaves of ore.xml that a screen "
                                     "shows: a label, a help line, a description, the default, "
                                     "the format and the ORE releases that introduced and removed "
                                     "each one."),
                context=sql_text("Parameter documentation for the engine settings screens"),
                artefact_type="ore_parameters",
                table="ores_dq_ore_parameters_artefact_tbl",
                columns=PARAMETER_COLUMNS, rows=parameter_rows))


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--check", action="store_true",
                        help="fail if the committed seed differs from the texts")
    args = parser.parse_args()
    all_rows = rows()
    undocumented = missing()
    if undocumented:
        print(f"{len(undocumented)} parameters have no text, for example "
              f"{', '.join(undocumented[:5])}.", file=sys.stderr)
        return 1
    content = render(all_rows)
    if args.check:
        current = ""
        if os.path.exists(SEED):
            with open(SEED) as f:
                current = f.read()
        if current != content:
            print(f"{SEED} no longer matches the texts; regenerate it.", file=sys.stderr)
            return 1
        print(f"{len(all_rows)} parameters have text and match the seed.")
        return 0
    with open(SEED, "w") as f:
        f.write(content)
    print(f"Wrote {SEED}: {len(all_rows)} parameters")
    return 0


if __name__ == "__main__":
    sys.exit(main())
