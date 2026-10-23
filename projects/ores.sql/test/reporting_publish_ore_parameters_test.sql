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
 * pgTAP tests for the ore parameter publish.
 *
 * Two datasets stage the vocabulary of ORE XML: the value domains, and the
 * parameters that name one of them. The publish writes each into the tenant a
 * bundle names:
 * - The value domains and the parameters receive the rows the datasets stage
 * - A parameter keeps the id its dataset gives it
 * - An insert-only publish skips every row the tenant holds
 * - A replace-all publish closes rows written earlier in the same transaction
 * - A parameter whose value domain is not published is refused
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/reporting_publish_ore_parameters_test.sql
 */

begin;

select plan(7);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temp table t_domains on commit drop as
select id from ores_dq_datasets_tbl
where code = 'ore.parameter_value_domains' and valid_to = ores_utility_infinity_timestamp_fn();

create temp table t_parameters on commit drop as
select id from ores_dq_datasets_tbl
where code = 'ore.parameters' and valid_to = ores_utility_infinity_timestamp_fn();

-- A database the system provisioning step has run on already holds both
-- datasets, so the test closes the rows first and publishes from nothing.
delete from ores_reporting_ore_parameters_tbl
where tenant_id = ores_utility_system_tenant_id_fn();
delete from ores_reporting_ore_parameter_value_domains_tbl
where tenant_id = ores_utility_system_tenant_id_fn();

select throws_ok(
    format($$select * from ores_reporting_publish_ore_parameters_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb)$$,
        (select id from t_parameters)),
    '23503',
    null,
    'the parameters are refused while their value domains are not published');

select is(
    (select sum(record_count)::int
     from ores_reporting_publish_ore_parameter_value_domains_from_dq_fn(
         (select id from t_domains), ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb)),
    (select count(*)::int from ores_dq_ore_parameter_value_domains_artefact_tbl
     where dataset_id = (select id from t_domains)),
    'the publish writes every value domain the dataset stages');

select is(
    (select sum(record_count)::int
     from ores_reporting_publish_ore_parameters_from_dq_fn(
         (select id from t_parameters), ores_utility_system_tenant_id_fn(), 'upsert', '{}'::jsonb)),
    (select count(*)::int from ores_dq_ore_parameters_artefact_tbl
     where dataset_id = (select id from t_parameters)),
    'the publish writes every parameter the dataset stages');

select is(
    (select count(*)::int from ores_reporting_ore_parameters_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select count(*)::int from ores_dq_ore_parameters_artefact_tbl
     where dataset_id = (select id from t_parameters)),
    'the tenant holds one current row for each staged parameter');

select is_empty(
    $$select a.id
      from ores_dq_ore_parameters_artefact_tbl a
      where a.dataset_id = (select id from t_parameters)
        and not exists (
            select 1 from ores_reporting_ore_parameters_tbl p
            where p.tenant_id = ores_utility_system_tenant_id_fn()
              and p.id = a.id and p.xpath = a.xpath and p.name = a.name
              and p.valid_to = ores_utility_infinity_timestamp_fn())$$,
    'every parameter keeps the id and the XPath its dataset gives it');

select results_eq(
    format($$select action from ores_reporting_publish_ore_parameters_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'insert_only', '{}'::jsonb)$$,
        (select id from t_parameters)),
    $$values ('skipped'::text)$$,
    'an insert-only publish skips every parameter the tenant holds');

select lives_ok(
    format($$select * from ores_reporting_publish_ore_parameters_from_dq_fn(%L::uuid, ores_utility_system_tenant_id_fn(), 'replace_all', '{}'::jsonb)$$,
        (select id from t_parameters)),
    'a replace-all publish closes rows written earlier in the same transaction');

select * from finish();

rollback;
