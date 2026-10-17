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
 * Report Error Scopes Population Script
 *
 * Seeds the report_error_scopes lookup for the system tenant. Four of the five
 * codes are the ORE engine's own structured-log group names, copied verbatim,
 * so that an error's scope is the engine's statement about it and not a
 * classification this system applies. The fifth, Report, is the scope an error
 * has when it is about the run itself, which is where an error that never
 * reached the engine lands.
 *
 * This script is idempotent.
 */

\echo '--- Report Error Scopes ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
begin
    if not exists (
        select 1 from ores_reporting_report_error_scopes_tbl
        where tenant_id = v_sys_tenant and code = 'Trade'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_report_error_scopes_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'Trade', v_sys_tenant, 0,
            'Trade',
            'On the one trade the error names, such as a trade that would not build.',
            1,
            current_user, 'system.initial_load', 'Seed report error scope: Trade'
        );
        raise debug 'Created report error scope: Trade';
    else
        raise debug 'Report error scope already exists: Trade';
    end if;

    if not exists (
        select 1 from ores_reporting_report_error_scopes_tbl
        where tenant_id = v_sys_tenant and code = 'Analytics'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_report_error_scopes_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'Analytics', v_sys_tenant, 0,
            'Analytics',
            'On the one analytic the error names, such as an analytic that failed to run.',
            2,
            current_user, 'system.initial_load', 'Seed report error scope: Analytics'
        );
        raise debug 'Created report error scope: Analytics';
    else
        raise debug 'Report error scope already exists: Analytics';
    end if;

    if not exists (
        select 1 from ores_reporting_report_error_scopes_tbl
        where tenant_id = v_sys_tenant and code = 'Curve'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_report_error_scopes_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'Curve', v_sys_tenant, 0,
            'Curve',
            'On the one curve the error names, such as a curve that would not build while the market was assembled.',
            3,
            current_user, 'system.initial_load', 'Seed report error scope: Curve'
        );
        raise debug 'Created report error scope: Curve';
    else
        raise debug 'Report error scope already exists: Curve';
    end if;

    if not exists (
        select 1 from ores_reporting_report_error_scopes_tbl
        where tenant_id = v_sys_tenant and code = 'Configuration'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_report_error_scopes_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'Configuration', v_sys_tenant, 0,
            'Configuration',
            'On the one input the error names, which belongs to no one trade.',
            4,
            current_user, 'system.initial_load', 'Seed report error scope: Configuration'
        );
        raise debug 'Created report error scope: Configuration';
    else
        raise debug 'Report error scope already exists: Configuration';
    end if;

    if not exists (
        select 1 from ores_reporting_report_error_scopes_tbl
        where tenant_id = v_sys_tenant and code = 'Report'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_report_error_scopes_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'Report', v_sys_tenant, 0,
            'Report',
            'Nowhere below the run: the error is about the run itself, and names no subject.',
            5,
            current_user, 'system.initial_load', 'Seed report error scope: Report'
        );
        raise debug 'Created report error scope: Report';
    else
        raise debug 'Report error scope already exists: Report';
    end if;
end $$;
