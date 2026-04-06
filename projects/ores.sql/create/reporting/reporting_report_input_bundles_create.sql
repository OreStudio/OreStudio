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

/**
 * Report Input Bundles Table
 *
 * An immutable artifact created by the assemble_bundle workflow step.
 * Records the object-storage keys where the MsgPack-serialised trades
 * and market data blobs produced during gather_trades and
 * gather_market_data are stored.
 *
 * One bundle is created per report instance execution.  The bundle is
 * consumed downstream by prepare_ore_package to repackage the data as
 * a tarball for the compute grid.
 */

create table if not exists "ores_reporting_report_input_bundles_tbl" (
    "id"                        uuid    not null,
    "tenant_id"                 uuid    not null,
    "report_instance_id"        uuid    not null,
    "definition_id"             uuid    not null,
    "trades_storage_key"        text    not null,
    "market_data_storage_key"   text    not null,
    "trade_count"               integer not null default 0,
    "series_count"              integer not null default 0,
    "created_at"                timestamp with time zone not null default current_timestamp,
    primary key (tenant_id, id),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid),
    check ("trades_storage_key" <> ''),
    check ("market_data_storage_key" <> '')
);

-- One bundle per report instance execution.
create unique index if not exists ores_reporting_report_input_bundles_instance_uniq_idx
on "ores_reporting_report_input_bundles_tbl" (tenant_id, report_instance_id);

-- Tenant index for listing all bundles within a tenant.
create index if not exists ores_reporting_report_input_bundles_tenant_idx
on "ores_reporting_report_input_bundles_tbl" (tenant_id);
