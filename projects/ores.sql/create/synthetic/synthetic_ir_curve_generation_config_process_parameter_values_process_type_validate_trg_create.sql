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

-- =============================================================================
-- Hand-written cross-entity validation for ir_curve_generation_config_process_parameter_value
-- =============================================================================
-- The codegen single-argument Validations mechanism cannot express a
-- two-column lookup, so this trigger is written by hand (same reasoning
-- as ir_curve_generation_config's own composite checks). A parameter
-- value row joins its parameter definition and its parent config: the
-- definition's process_type_code must equal the config's process_type --
-- a config driven by TWO_FACTOR_GAUSSIAN may only carry values whose
-- definitions belong to TWO_FACTOR_GAUSSIAN, or a row could smuggle in a
-- parameter for a different model than the one the mapping layer will
-- dispatch on. Fires on insert and on update of either key column, so a
-- late retargeting of an existing row is checked too.
--
-- The definitions are system-tenant reference data (seeded by
-- synthetic_yield_curve_process_parameter_definitions_populate.sql under
-- the system tenant, like the process types themselves, whose validation
-- function resolves against ores_utility_system_tenant_id_fn()); the
-- definition lookup therefore resolves there, while the parent config --
-- a genuine tenant row -- resolves against the value row's own tenant.

create or replace function ores_synthetic_validate_process_parameter_value_process_type_fn()
returns trigger as $$
declare
    v_definition_process_type text;
    v_config_process_type text;
begin
    -- Resolve the process type of the referenced parameter definition.
    select "process_type_code" into v_definition_process_type
    from "ores_synthetic_process_parameter_definitions_tbl"
    where tenant_id = ores_utility_system_tenant_id_fn()
      and id = NEW.parameter_definition_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_definition_process_type is null then
        raise exception 'Invalid parameter_definition_id: %. No active yield_curve_process_parameter_definition found with this id.', NEW.parameter_definition_id
            using errcode = '23503';
    end if;

    -- Resolve the process type of the parent config.
    select "process_type" into v_config_process_type
    from "ores_synthetic_ir_curve_generation_configs_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.config_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if v_config_process_type is null then
        raise exception 'Invalid config_id: %. No active ir_curve_generation_config found with this id.', NEW.config_id
            using errcode = '23503';
    end if;

    if v_definition_process_type <> v_config_process_type then
        raise exception 'parameter_definition_id % belongs to process type %, which does not match config %''s process type %.', NEW.parameter_definition_id, v_definition_process_type, NEW.config_id, v_config_process_type
            using errcode = '23503';
    end if;

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_synthetic_config_process_parameter_values_process_type_trg
before insert or update of config_id, parameter_definition_id on "ores_synthetic_config_process_parameter_values_tbl"
for each row execute function ores_synthetic_validate_process_parameter_value_process_type_fn();
