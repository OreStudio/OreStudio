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
 * One-shot migration: flat process-parameter columns -> row-based store
 *
 * The ir_curve_generation_config table once carried its process
 * parameters as flat scalar columns (kappa, theta, sigma, initial_rate),
 * which hardcoded a 1-factor model shape. Those columns have been
 * removed from the schema; the parameters now live as
 * ir_curve_generation_config_process_parameter_value rows referencing
 * the yield_curve_process_parameter_definition catalogue, exactly the
 * shape the mapping layer (map_parameters_to_yield_curve_process) and
 * the UI consume.
 *
 * PREREQUISITES (against a database that has NOT been recreated from
 * the new scripts):
 *   1. The new create scripts have been applied (they are idempotent:
 *      create table if not exists / create or replace function).
 *   2. The foundation populate has run, seeding the process types and
 *      the parameter definitions catalogue (both required for the FK
 *      resolution below).
 *
 * On a freshly recreated database the flat columns do not exist and
 * this script is a guarded no-op.
 *
 * This script is idempotent.
 */

\echo '--- Migrating flat process parameters to row-based store ---'

do $$
declare
    v_has_flat_columns boolean;
    v_constraint_name text;
begin
    select exists (
        select 1 from information_schema.columns
        where table_name = 'ores_synthetic_ir_curve_generation_configs_tbl'
          and column_name = 'kappa'
    ) into v_has_flat_columns;

    if not v_has_flat_columns then
        raise notice 'ir_curve_generation_config has no flat kappa column; migration not needed.';
        return;
    end if;

    -- 1. Convert every existing config's flat parameter columns into
    --    parameter-value rows, resolving each parameter's definition id
    --    from the system-tenant definitions catalogue (same lookup the
    --    values trigger uses). The config's process_type selects the
    --    catalogue: only rows of that type resolve, so a row referencing
    --    a definition of another type would raise -- it cannot, because
    --    the flat columns predate the row-based store and every existing
    --    config predates TWO_FACTOR_GAUSSIAN.
    insert into ores_synthetic_config_process_parameter_values_tbl (
        tenant_id, id, version, config_id, parameter_definition_id, parameter_value,
        modified_by, performed_by, change_reason_code, change_commentary
    )
    select
        c.tenant_id, gen_random_uuid(), 0, c.id, d.id, p.parameter_value,
        coalesce(ores_iam_current_service_fn(), current_user), current_user,
        'system.data_migration', 'Migrated from flat parameter columns'
    from ores_synthetic_ir_curve_generation_configs_tbl c
    cross join lateral (values
        ('kappa', c.kappa),
        ('theta', c.theta),
        ('sigma', c.sigma),
        ('initial_rate', c.initial_rate)
    ) as p(parameter_name, parameter_value)
    join ores_synthetic_process_parameter_definitions_tbl d
        on d.tenant_id = ores_utility_system_tenant_id_fn()
       and d.process_type_code = c.process_type
       and d.parameter_name = p.parameter_name
       and d.valid_to = ores_utility_infinity_timestamp_fn()
    where c.valid_to = ores_utility_infinity_timestamp_fn();

    -- 2. Drop the two superseded CHECK constraints. They were declared
    --    inline and therefore auto-named, so match by definition text
    --    rather than by guessing the generated name.
    for v_constraint_name in
        select conname
        from pg_constraint
        where conrelid = 'ores_synthetic_ir_curve_generation_configs_tbl'::regclass
          and contype = 'c'
          and (
              pg_get_constraintdef(oid) like '%"sigma" >= 0%'
              or pg_get_constraintdef(oid) like '%"initial_rate" >= 0%'
          )
    loop
        execute format(
            'alter table ores_synthetic_ir_curve_generation_configs_tbl drop constraint %I',
            v_constraint_name);
    end loop;

    -- 3. Drop the flat columns.
    alter table ores_synthetic_ir_curve_generation_configs_tbl
        drop column if exists kappa,
        drop column if exists theta,
        drop column if exists sigma,
        drop column if exists initial_rate;
end $$;

-- Summary
select 'ores_synthetic_ir_curve_generation_configs_tbl' as entity, count(*) as parameter_value_rows
from ores_synthetic_config_process_parameter_values_tbl;
