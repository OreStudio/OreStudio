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
 * Report Instance Kinds Population Script
 *
 * Seeds the report_instance_kinds lookup for the system tenant with the two kinds of
 * report instance: a production run, and a package build that stops after it has
 * prepared the engine's input. This script is idempotent.
 */

\echo '--- Report Instance Kinds ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
begin
    if not exists (
        select 1 from ores_reporting_report_instance_kinds_tbl
        where tenant_id = v_sys_tenant and code = 'production'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_report_instance_kinds_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'production', v_sys_tenant, 0,
            'Production',
            'Prepares the engine''s input, computes it on the compute grid and records the results. The scheduler creates this kind of instance.',
            1,
            current_user, 'system.initial_load', 'Seed report instance kind: production'
        );
        raise debug 'Created report instance kind: production';
    else
        raise debug 'Report instance kind already exists: production';
    end if;

    if not exists (
        select 1 from ores_reporting_report_instance_kinds_tbl
        where tenant_id = v_sys_tenant and code = 'package'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_report_instance_kinds_tbl (
            code, tenant_id, version,
            name, description, display_order,
            modified_by, change_reason_code, change_commentary
        ) values (
            'package', v_sys_tenant, 0,
            'Package',
            'Prepares the engine''s input and stops. It produces the packages the compute grid would receive for the report and computes nothing, so they can be inspected and run in ORE locally.',
            2,
            current_user, 'system.initial_load', 'Seed report instance kind: package'
        );
        raise debug 'Created report instance kind: package';
    else
        raise debug 'Report instance kind already exists: package';
    end if;
end $$;
