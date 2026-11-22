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
 * pgTAP tests for the versions of the published script library.
 *
 * The bring-up publishes the ore.script_library dataset into the system tenant.
 * A change to one staged script, published again:
 * - Every current row of every script carries its script's current version
 * - The changed script gets a new version, and every current row of it
 *   carries that version
 * - The rows of the earlier version still read back as the script it was
 * - The current rows read back as the staged script
 * - A script that did not change keeps its version
 *
 * Run with: pg_prove -d <database> test/trading_script_library_versions_test.sql
 */

begin;

select plan(7);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp table t_ctx on commit drop as
select (select id from ores_dq_datasets_tbl
        where code = 'ore.script_library'
          and valid_to = ores_utility_infinity_timestamp_fn()) as dataset_id,
       (select version from ores_trading_scripts_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and name = 'Accumulator01'
          and valid_to = ores_utility_infinity_timestamp_fn()) as before_version,
       (select version from ores_trading_scripts_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and name = 'AsianBasketOption'
          and valid_to = ores_utility_infinity_timestamp_fn()) as other_version,
       ores_trading_published_script_state_fn(
           ores_utility_system_tenant_id_fn(), 'Accumulator01') as before_state;

create temp view t_stale_rows as
select k.table_name, k.script_name, k.script_version, s.version
from (
    select 'variants' as table_name, script_name, script_version, tenant_id
    from ores_trading_script_variants_tbl
    where valid_to = ores_utility_infinity_timestamp_fn()
    union all select 'results', script_name, script_version, tenant_id
    from ores_trading_script_results_tbl
    where valid_to = ores_utility_infinity_timestamp_fn()
    union all select 'calibrations', script_name, script_version, tenant_id
    from ores_trading_script_calibrations_tbl
    where valid_to = ores_utility_infinity_timestamp_fn()
    union all select 'strikes', script_name, script_version, tenant_id
    from ores_trading_script_calibration_strikes_tbl
    where valid_to = ores_utility_infinity_timestamp_fn()
    union all select 'coarsenings', script_name, script_version, tenant_id
    from ores_trading_script_coarsenings_tbl
    where valid_to = ores_utility_infinity_timestamp_fn()
    union all select 'inputs', script_name, script_version, tenant_id
    from ores_trading_script_inputs_tbl
    where valid_to = ores_utility_infinity_timestamp_fn()
) k
join ores_trading_scripts_tbl s
  on s.tenant_id = k.tenant_id and s.name = k.script_name
 and s.valid_to = ores_utility_infinity_timestamp_fn()
where k.tenant_id = ores_utility_system_tenant_id_fn()
  and k.script_version <> s.version;

select is(
    (select count(*) from t_stale_rows),
    0::bigint,
    'every current row of every script carries its script''s current version');

update ores_dq_script_inputs_artefact_tbl set description = 'changed for the test'
where dataset_id = (select dataset_id from t_ctx)
  and script_name = 'Accumulator01' and sequence_number = 1;
delete from ores_dq_script_results_artefact_tbl
where dataset_id = (select dataset_id from t_ctx)
  and script_name = 'Accumulator01' and sequence_number = 4;

select ores_trading_publish_script_library_from_dq_fn(
    (select dataset_id from t_ctx), ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

select is(
    (select version from ores_trading_scripts_tbl
     where tenant_id = ores_utility_system_tenant_id_fn() and name = 'Accumulator01'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select before_version + 1 from t_ctx),
    'the changed script gets a new version');

select is(
    (select count(*) from t_stale_rows),
    0::bigint,
    'every current row of the changed script carries its new version');

select is(
    ores_trading_published_script_state_fn(
        ores_utility_system_tenant_id_fn(), 'Accumulator01',
        (select before_version from t_ctx)),
    (select before_state from t_ctx),
    'the rows of the earlier version read back as the script it was');

select isnt(
    ores_trading_published_script_state_fn(
        ores_utility_system_tenant_id_fn(), 'Accumulator01'),
    (select before_state from t_ctx),
    'the current rows are not the earlier script');

select is(
    ores_trading_published_script_state_fn(
        ores_utility_system_tenant_id_fn(), 'Accumulator01'),
    ores_trading_staged_script_state_fn((select dataset_id from t_ctx), 'Accumulator01'),
    'the current rows read back as the staged script');

select is(
    (select version from ores_trading_scripts_tbl
     where tenant_id = ores_utility_system_tenant_id_fn() and name = 'AsianBasketOption'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select other_version from t_ctx),
    'a script that did not change keeps its version');

select * from finish();
rollback;
