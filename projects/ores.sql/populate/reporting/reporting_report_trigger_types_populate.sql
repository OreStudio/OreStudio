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
 * Report Trigger Types Population Script
 *
 * Seeds the report_trigger_types lookup for the system tenant with the two types
 * of report trigger: a run a person asks for now, and a run a schedule asks for
 * later. This script is idempotent.
 */

\echo '--- Report Trigger Types ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
begin
    if not exists (
        select 1 from ores_reporting_report_trigger_types_tbl
        where tenant_id = v_sys_tenant and code = 'on_demand'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_report_trigger_types_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'on_demand', v_sys_tenant, 0,
            'On Demand',
            'A run a person asks for now, by naming the report within the party they are signed in to. It names no scheduler job and no firing.',
            1,
            current_user, 'system.initial_load', 'Seed report trigger type: on_demand'
        );
        raise debug 'Created report trigger type: on_demand';
    else
        raise debug 'Report trigger type already exists: on_demand';
    end if;

    if not exists (
        select 1 from ores_reporting_report_trigger_types_tbl
        where tenant_id = v_sys_tenant and code = 'scheduled'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_report_trigger_types_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'scheduled', v_sys_tenant, 0,
            'Scheduled',
            'A run a schedule asks for later. The scheduler job publishes a trigger event when it fires, and the trigger names the job and the firing that produced it.',
            2,
            current_user, 'system.initial_load', 'Seed report trigger type: scheduled'
        );
        raise debug 'Created report trigger type: scheduled';
    else
        raise debug 'Report trigger type already exists: scheduled';
    end if;
end $$;
