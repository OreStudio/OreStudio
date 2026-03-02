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
 * Report Types Population Script
 *
 * Seeds the report_types enum table for the system tenant.
 * This script is idempotent.
 */

\echo '--- Report Types ---'

do $$
declare
    v_sys_tenant uuid := ores_iam_system_tenant_id_fn();
begin
    if not exists (
        select 1 from ores_reporting_report_types_tbl
        where tenant_id = v_sys_tenant and code = 'risk'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_report_types_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'risk', v_sys_tenant, 0,
            'Risk Report',
            'Full ORE risk analytics run: NPV, cashflows, sensitivities, XVA, VaR, and SIMM.',
            1,
            current_user, 'system.initial_load', 'Seed report type: risk'
        );
        raise notice 'Created report type: risk';
    else
        raise notice 'Report type already exists: risk';
    end if;

    if not exists (
        select 1 from ores_reporting_report_types_tbl
        where tenant_id = v_sys_tenant and code = 'grid'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_report_types_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'grid', v_sys_tenant, 0,
            'Grid Report',
            'Tabular portfolio data export without analytics computation.',
            2,
            current_user, 'system.initial_load', 'Seed report type: grid'
        );
        raise notice 'Created report type: grid';
    else
        raise notice 'Report type already exists: grid';
    end if;
end;
$$;

select 'Report Types' as entity, count(*) as count
from ores_reporting_report_types_tbl
where tenant_id = ores_iam_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
