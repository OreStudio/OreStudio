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
 * ORE Application Seed Script
 *
 * Seeds the ORE (Open Source Risk Engine) application and its standard app
 * version in the compute grid.  Uses stable UUIDs so the script is fully
 * idempotent — safe to run on every database recreation.
 *
 * Stable UUIDs:
 *   ORE app:         c0ffee00-0000-0000-0000-000000000001
 *   ORE app_version: c0ffee00-0000-0000-0000-000000000002
 *
 * The system tenant (ffffffff-ffff-ffff-ffff-ffffffffffff) owns these
 * records so they are visible to all tenants through RLS.
 */

\echo '--- ORE Application Seed ---'

-- -------------------------------------------------------------------------
-- ORE Application
-- -------------------------------------------------------------------------
insert into ores_compute_apps_tbl (
    id,
    tenant_id,
    version,
    name,
    description,
    modified_by,
    performed_by,
    change_reason_code,
    change_commentary,
    valid_from,
    valid_to
)
select
    'c0ffee00-0000-0000-0000-000000000001'::uuid,
    ores_iam_system_tenant_id_fn(),
    1,
    'ORE',
    'Open Source Risk Engine — QuantLib-based risk analytics engine for '
    'market risk, counterparty credit risk, XVA, and sensitivity analysis.',
    'system',
    'system',
    'system.new_record',
    '',
    current_timestamp,
    ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_compute_apps_tbl
    where id = 'c0ffee00-0000-0000-0000-000000000001'::uuid
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- -------------------------------------------------------------------------
-- ORE App Version 9.0.0 (linux-x86_64)
-- -------------------------------------------------------------------------
insert into ores_compute_app_versions_tbl (
    id,
    tenant_id,
    version,
    app_id,
    wrapper_version,
    engine_version,
    package_uri,
    min_ram_mb,
    modified_by,
    performed_by,
    change_reason_code,
    change_commentary,
    valid_from,
    valid_to
)
select
    'c0ffee00-0000-0000-0000-000000000002'::uuid,
    ores_iam_system_tenant_id_fn(),
    1,
    'c0ffee00-0000-0000-0000-000000000001'::uuid,
    '1.0.0',
    '9.0.0',
    'https://ore.example.com/packages/ore-9.0.0-linux-x86_64.tar.gz',
    512,
    'system',
    'system',
    'system.new_record',
    '',
    current_timestamp,
    ores_utility_infinity_timestamp_fn()
where not exists (
    select 1 from ores_compute_app_versions_tbl
    where id = 'c0ffee00-0000-0000-0000-000000000002'::uuid
      and valid_to = ores_utility_infinity_timestamp_fn()
);

-- -------------------------------------------------------------------------
-- ORE App Version Platform (linux-x86_64)
-- -------------------------------------------------------------------------
insert into ores_compute_app_version_platforms_tbl (app_version_id, platform_code)
values ('c0ffee00-0000-0000-0000-000000000002'::uuid, 'linux-x86_64')
on conflict (app_version_id, platform_code) do nothing;
