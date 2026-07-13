/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
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
-- One row per (crm_name, pair) the target party's CRM should have. A
-- named CRM (crm_topology_config) itself is implied by the distinct set
-- of crm_name values -- pivot_currency_code is repeated per row rather
-- than requiring a separate config-level artefact row, since every row
-- for a given crm_name always agrees on its pivot. row_kind distinguishes
-- a spanning-tree edge (crm_driver_pair) from a curated non-edge cross a
-- consumer may still request (crm_enabled_derived_pair) -- see the
-- publish-from-dq function that expands this into both target tables.
create table if not exists "ores_dq_crm_topology_bundles_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "crm_name" text not null,
    "pivot_currency_code" text not null,
    "base_currency_code" text not null,
    "quote_currency_code" text not null,
    "row_kind" text not null,
    check ("row_kind" in ('driver', 'derived')),
    check ("base_currency_code" <> ''),
    check ("quote_currency_code" <> ''),
    check ("base_currency_code" <> "quote_currency_code"),
    check ("crm_name" <> ''),
    check ("pivot_currency_code" <> '')
);

create index if not exists crm_topology_bundles_artefact_dataset_idx
on "ores_dq_crm_topology_bundles_artefact_tbl" (dataset_id);

create index if not exists crm_topology_bundles_artefact_tenant_idx
on "ores_dq_crm_topology_bundles_artefact_tbl" (tenant_id);

create index if not exists crm_topology_bundles_artefact_name_idx
on "ores_dq_crm_topology_bundles_artefact_tbl" (dataset_id, crm_name);
