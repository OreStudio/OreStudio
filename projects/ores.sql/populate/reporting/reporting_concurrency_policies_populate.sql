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
 * Concurrency Policies Population Script
 *
 * Seeds the concurrency_policies enum table for the system tenant.
 * This script is idempotent.
 */

\echo '--- Concurrency Policies ---'

do $$
declare
    v_sys_tenant uuid := ores_iam_system_tenant_id_fn();
begin
    if not exists (
        select 1 from ores_reporting_concurrency_policies_tbl
        where tenant_id = v_sys_tenant and code = 'skip'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_concurrency_policies_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'skip', v_sys_tenant, 0,
            'Skip',
            'If a report instance is already running, skip the new trigger. The new instance is written directly as terminal (skipped).',
            1,
            current_user, 'system.initial_load', 'Seed concurrency policy: skip'
        );
        raise notice 'Created concurrency policy: skip';
    else
        raise notice 'Concurrency policy already exists: skip';
    end if;

    if not exists (
        select 1 from ores_reporting_concurrency_policies_tbl
        where tenant_id = v_sys_tenant and code = 'queue'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_concurrency_policies_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'queue', v_sys_tenant, 0,
            'Queue',
            'If a report instance is already running, queue the new trigger. The new instance starts in the queued state and is promoted when the running instance completes.',
            2,
            current_user, 'system.initial_load', 'Seed concurrency policy: queue'
        );
        raise notice 'Created concurrency policy: queue';
    else
        raise notice 'Concurrency policy already exists: queue';
    end if;

    if not exists (
        select 1 from ores_reporting_concurrency_policies_tbl
        where tenant_id = v_sys_tenant and code = 'fail'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_concurrency_policies_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'fail', v_sys_tenant, 0,
            'Fail',
            'If a report instance is already running, immediately fail the new trigger. The new instance is written directly as terminal (failed).',
            3,
            current_user, 'system.initial_load', 'Seed concurrency policy: fail'
        );
        raise notice 'Created concurrency policy: fail';
    else
        raise notice 'Concurrency policy already exists: fail';
    end if;
end;
$$;

select 'Concurrency Policies' as entity, count(*) as count
from ores_reporting_concurrency_policies_tbl
where tenant_id = ores_iam_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
