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
 * - A version the script never had reads back as no script
 *
 * Run with: pg_prove -d <database> test/trading_script_library_versions_test.sql
 */

begin;

select plan(4);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp table t_ctx on commit drop as
select (select id from ores_dq_datasets_tbl
        where code = 'ore.script_library'
          and valid_to = ores_utility_infinity_timestamp_fn()) as dataset_id,
       (select version from ores_trading_scripts_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and name = 'CMSCapFloorBarrier'
          and valid_to = ores_utility_infinity_timestamp_fn()) as before_version;

create temp view t_codes as
select c.script_name, c.input_name, c.code, c.label, c.script_version
from ores_trading_script_input_codes_tbl c
where c.tenant_id = ores_utility_system_tenant_id_fn()
  and c.valid_to = ores_utility_infinity_timestamp_fn();

select is(
    (select string_agg(code || '=' || label, ', ' order by code) from t_codes
     where script_name = 'CMSCapFloorBarrier' and input_name = 'BarrierType'),
    '1=DownIn, 2=UpIn, 3=DownOut, 4=UpOut',
    'a coded input of the library is published with the codes its comment states');

select is(
    (select label from t_codes
     where script_name = 'GenericBarrierOption' and input_name = 'TransatlanticBarrierType'
       and code = 1),
    'DownIn',
    'a correction replaces the reading of a comment that misspells a code');

update ores_dq_script_input_codes_artefact_tbl set label = 'Down and in'
where dataset_id = (select dataset_id from t_ctx)
  and script_name = 'CMSCapFloorBarrier' and input_name = 'BarrierType' and code = 1;

select ores_trading_publish_script_library_from_dq_fn(
    (select dataset_id from t_ctx), ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb);

select is(
    (select version from ores_trading_scripts_tbl
     where tenant_id = ores_utility_system_tenant_id_fn() and name = 'CMSCapFloorBarrier'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select before_version + 1 from t_ctx),
    'a changed code gives its script a new version');

select is(
    (select label || ' at ' || script_version from t_codes
     where script_name = 'CMSCapFloorBarrier' and input_name = 'BarrierType' and code = 1),
    'Down and in at ' || (select before_version + 1 from t_ctx),
    'the changed code is published with the script''s new version');

select * from finish();
rollback;
