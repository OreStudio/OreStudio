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
 * Parameter Value Domains Population Script
 *
 * Seeds the storage kinds a parameter definition can name. The run document's
 * analytics use the first five; referenced_entity is empty until a domain names
 * an entity rather than a primitive. This script is idempotent.
 */

\echo '--- Parameter Value Domains ---'

do $$
declare
    v_sys_tenant uuid := ores_utility_system_tenant_id_fn();
begin
    if not exists (
        select 1 from ores_reporting_parameter_value_domains_tbl
        where tenant_id = v_sys_tenant and code = 'string'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_value_domains_tbl (
            code, tenant_id, version,
            name, storage_kind, referenced_entity,
            modified_by, change_reason_code, change_commentary
        ) values (
            'string', v_sys_tenant, 0,
            'Text', 'string', '',
            current_user, 'system.initial_load', 'Free text, as ORE spells it.'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_value_domains_tbl
        where tenant_id = v_sys_tenant and code = 'boolean'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_value_domains_tbl (
            code, tenant_id, version,
            name, storage_kind, referenced_entity,
            modified_by, change_reason_code, change_commentary
        ) values (
            'boolean', v_sys_tenant, 0,
            'Boolean', 'boolean', '',
            current_user, 'system.initial_load', 'A flag ORE writes as Y/N or true/false; the spelling is kept.'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_value_domains_tbl
        where tenant_id = v_sys_tenant and code = 'integer'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_value_domains_tbl (
            code, tenant_id, version,
            name, storage_kind, referenced_entity,
            modified_by, change_reason_code, change_commentary
        ) values (
            'integer', v_sys_tenant, 0,
            'Integer', 'integer', '',
            current_user, 'system.initial_load', 'A whole number.'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_value_domains_tbl
        where tenant_id = v_sys_tenant and code = 'double'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_value_domains_tbl (
            code, tenant_id, version,
            name, storage_kind, referenced_entity,
            modified_by, change_reason_code, change_commentary
        ) values (
            'double', v_sys_tenant, 0,
            'Decimal', 'double', '',
            current_user, 'system.initial_load', 'A decimal number.'
        );
    end if;

    if not exists (
        select 1 from ores_reporting_parameter_value_domains_tbl
        where tenant_id = v_sys_tenant and code = 'date'
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        insert into ores_reporting_parameter_value_domains_tbl (
            code, tenant_id, version,
            name, storage_kind, referenced_entity,
            modified_by, change_reason_code, change_commentary
        ) values (
            'date', v_sys_tenant, 0,
            'Date', 'date', '',
            current_user, 'system.initial_load', 'An ISO date.'
        );
    end if;

end;
$$;

select 'Parameter Value Domains' as entity, count(*) as count
from ores_reporting_parameter_value_domains_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn();
