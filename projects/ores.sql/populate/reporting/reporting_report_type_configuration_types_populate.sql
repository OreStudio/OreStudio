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
 * Report Type Configuration Types Population Script
 *
 * Seeds, for the system tenant, the configuration types each report type
 * requires. This script is idempotent.
 *
 * A risk run cannot start without pricing engines, today's market, curve
 * configuration and conventions, whatever analytics it asks for: ORE reads
 * all four before it prices anything. Analytic-specific configuration, such
 * as a simulation or a sensitivity set, is not required by the type, because
 * a risk report that does not ask for that analytic does not need it.
 */

\echo '--- Report Type Configuration Types ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
    r record;
begin
    for r in
        select * from (values
            ('risk', 'pricing_engines'),
            ('risk', 'todays_market'),
            ('risk', 'curve_configuration'),
            ('risk', 'conventions')
        ) as t(report_type_code, configuration_type_code)
    loop
        if not exists (
            select 1 from ores_reporting_report_type_configuration_types_tbl
            where tenant_id = v_sys_tenant
              and report_type_code = r.report_type_code
              and configuration_type_code = r.configuration_type_code
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            insert into ores_reporting_report_type_configuration_types_tbl (
                report_type_code, tenant_id, configuration_type_code, version,
                modified_by, change_reason_code, change_commentary
            ) values (
                r.report_type_code, v_sys_tenant, r.configuration_type_code, 0,
                current_user, 'system.initial_load',
                'Seed requirement: ' || r.report_type_code || ' requires '
                    || r.configuration_type_code
            );
            raise debug 'Created requirement: % requires %',
                r.report_type_code, r.configuration_type_code;
        end if;
    end loop;
end;
$$;

select 'Report Type Configuration Types' as entity, count(*) as count
from ores_reporting_report_type_configuration_types_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
