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
"""Shared pieces of the generator that writes the ore parameter dataset.

A parameter row is a dict with the keys scope, subtype, name, position, domain,
required, the text keys label, help, description, default_text, format and
introduced_in, and removed_in. The schema name and the XPath follow from the
scope, subtype and name.
"""

import uuid

SCHEMA_NAME = "ore.xsd"

BLOCK_SCOPES = ("Setup", "Logging", "Markets")

TEXT_KEYS = ("label", "help", "description", "default_text", "format", "introduced_in")

# The staging key of a parameter. It is derived from the leaf the row documents, so a row
# keeps its id in every tenant it is published to and in every later regeneration.
ID_NAMESPACE = uuid.UUID("5d1c2c0e-6b7e-5a43-9d66-0b6c1f2a7e91")


def sql_text(value):
    if value is None:
        return "null"
    return "'" + value.replace("'", "''") + "'"


def xpath(row):
    if row["scope"] in BLOCK_SCOPES:
        return f"/ORE/{row['scope']}/Parameter[@name='{row['name']}']"
    return (f"/ORE/Analytics/Analytic[@type='{row['subtype']}']"
            f"/Parameter[@name='{row['name']}']")


def parameter_id(row):
    return uuid.uuid5(ID_NAMESPACE, f"{SCHEMA_NAME}|{xpath(row)}")


def artefact_row(row):
    """The values clause of one parameter in the staging table."""
    text = [sql_text(row.get(k)) for k in TEXT_KEYS]
    required = "true" if row.get("required") else "false"
    return (f"        (v_dataset_id, ores_utility_system_tenant_id_fn(), "
            f"'{parameter_id(row)}', 0, "
            f"{sql_text(row['scope'])}, {sql_text(row['subtype'])}, {sql_text(row['name'])}, "
            f"{sql_text(SCHEMA_NAME)}, {sql_text(xpath(row))}, {row['position']}, "
            f"{sql_text(row['domain'])}, {required}, "
            f"{text[0]}, {text[1]}, {text[2]}, {text[3]}, {text[4]}, {text[5]}, "
            f"{sql_text(row.get('removed_in'))})")
