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
 * pgTAP tests for the risk report configuration publish.
 *
 * A dataset stages a risk report configuration with the name of the portfolio it
 * reads, its analytics and the values of their parameters. The publish writes
 * them for the party a publish names:
 * - The configuration, its analytics, its parameters and the reporting
 *   configuration it is registered as receive their rows, and the ids are re-keyed
 *   for the party
 * - A staged parameter names the parameter, and the publish resolves it to the
 *   definition of the analytic type the staged analytic has
 * - The run settings the configuration states, and the market bindings it holds,
 *   reach the live tables as staged
 * - The portfolio scope names the portfolio the party holds under that name
 * - A repeat publish adds nothing
 * - A parameter that no definition describes is refused, and so is a
 *   configuration with no analytic
 * - The analytics of a configuration make a run only when one is on and each
 *   analytic that reads the output of another has that analytic on
 *
 * Run with: pg_prove -d <database> test/reporting_publish_risk_report_configs_test.sql
 */

begin;

select plan(16);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       '00000000-0000-0000-0000-0000000e0001'::uuid as config_id,
       '00000000-0000-0000-0000-0000000e0002'::uuid as configuration_id,
       '00000000-0000-0000-0000-0000000e0003'::uuid as portfolio_id,
       '00000000-0000-0000-0000-0000000e0004'::uuid as simulation_id,
       '00000000-0000-0000-0000-0000000e0005'::uuid as pfe_id,
       '00000000-0000-0000-0000-0000000e0006'::uuid as parameter_id,
       '00000000-0000-0000-0000-0000000e0007'::uuid as empty_config_id,
       '00000000-0000-0000-0000-0000000e0008'::uuid as lonely_configuration_id,
       '00000000-0000-0000-0000-0000000e000a'::uuid as market_id;

insert into ores_refdata_portfolios_tbl (
    id, tenant_id, version, party_id, name, parent_portfolio_id, purpose_type,
    is_virtual, sandbox_id, status, modified_by, performed_by,
    change_reason_code, change_commentary
)
select c.portfolio_id, ores_utility_system_tenant_id_fn(), 0, c.party_id,
    'RRC-FIXTURE-PORTFOLIO', null, 'Risk', false, null, 'Active',
    current_user, current_user, 'system.test', 'Risk report configuration pgTAP fixture'
from t_ctx c;

do $$
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.risk_report_configs', 'ACME Corporation', 'Reporting', 'Reference Data', 'NONE',
        'Primary', 'Synthetic', 'Raw', 'OreStudio Code Generation Methodology',
        'Test Risk Report Configurations', 'A risk report configuration staged for a test.',
        'ACMEBANK', 'pgTAP fixture', current_date, 'Internal Use Only', 'risk_report_configs');
end $$;

create temp table t_dataset on commit drop as
select id from ores_dq_datasets_tbl
where code = 'test.risk_report_configs' and valid_to = ores_utility_infinity_timestamp_fn();

insert into ores_dq_risk_report_configs_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, name, description, configuration_id,
    base_currency, observation_model, n_threads, market_data_type, input_path, output_path,
    log_file, log_mask, imply_todays_fixings, scope_portfolio)
select d.id, ores_utility_system_tenant_id_fn(), c.config_id, 0, c.party_id,
    'Test exposure', 'A risk report configuration', c.configuration_id,
    'GBP', 'Disable', 1, 'eod', 'Input', 'Output', 'log.txt', 31, 'N', 'RRC-FIXTURE-PORTFOLIO'
from t_dataset d, t_ctx c;

insert into ores_dq_configuration_analytics_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, configuration_id, analytic_type_code,
    display_order, active)
select d.id, ores_utility_system_tenant_id_fn(), a.id, 0, c.party_id, c.configuration_id,
    a.analytic_type_code, a.display_order, 'Y'
from t_dataset d, t_ctx c,
     lateral (values (c.simulation_id, 'simulation', 1), (c.pfe_id, 'pfe', 2))
         as a(id, analytic_type_code, display_order);

insert into ores_dq_configuration_analytic_parameters_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, configuration_analytic_id, parameter_name,
    value, position)
select d.id, ores_utility_system_tenant_id_fn(), c.parameter_id, 0, c.party_id, c.pfe_id,
    'quantile', '0.95', 1
from t_dataset d, t_ctx c;

insert into ores_dq_configuration_market_bindings_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, configuration_id, role,
    market_configuration_name, position)
select d.id, ores_utility_system_tenant_id_fn(), c.market_id, 0, c.party_id, c.configuration_id,
    'pricing', 'default', 1
from t_dataset d, t_ctx c;

create or replace function pg_temp.publish() returns table (action text, record_count bigint) as $$
    select p.action, p.record_count
    from t_dataset d, t_ctx c,
         ores_reporting_publish_risk_report_configs_from_dq_fn(
             d.id, ores_utility_system_tenant_id_fn(),
             'upsert', jsonb_build_object('party_id', c.party_id::text)) p
    order by p.action;
$$ language sql;

select results_eq(
    $$select * from pg_temp.publish()$$,
    $$values ('inserted'::text, 5::bigint), ('scoped'::text, 1::bigint)$$,
    'the publish adds the configuration, its two analytics, its parameter, its market binding and its scope');

create temp view t_live as
select r.*
from ores_reporting_risk_report_configs_tbl r, t_ctx c
where r.tenant_id = ores_utility_system_tenant_id_fn() and r.party_id = c.party_id
  and r.name = 'Test exposure' and r.valid_to = ores_utility_infinity_timestamp_fn();

select is(
    (select r.id from t_live r),
    ores_dq_party_uuid_fn((select party_id from t_ctx), (select config_id from t_ctx)),
    'the configuration is re-keyed for the party');

select is(
    (select r.base_currency from t_live r),
    'GBP',
    'the configuration keeps its settings');

select results_eq(
    $$select r.observation_model, r.input_path, r.output_path, r.log_file, r.log_mask,
             r.imply_todays_fixings, r.continue_on_error
      from t_live r$$,
    $$values ('Disable'::text, 'Input'::text, 'Output'::text, 'log.txt'::text, 31,
              'N'::text, null::text)$$,
    'the configuration keeps the run choices it states and leaves the others empty');

select results_eq(
    $$select b.role, b.market_configuration_name, b.position
      from ores_reporting_configuration_market_bindings_tbl b, t_live r
      where b.tenant_id = ores_utility_system_tenant_id_fn()
        and b.configuration_id = r.configuration_id
        and b.valid_to = ores_utility_infinity_timestamp_fn()$$,
    $$values ('pricing'::text, 'default'::text, 1)$$,
    'the market binding belongs to the configuration');

select is(
    (select r.configuration_id from t_live r),
    ores_dq_party_uuid_fn((select party_id from t_ctx), (select configuration_id from t_ctx)),
    'the reporting configuration id is re-keyed for the party');

select is(
    (select count(*) from ores_reporting_configurations_tbl g, t_live r
     where g.tenant_id = ores_utility_system_tenant_id_fn()
       and g.id = r.configuration_id
       and g.configuration_type_code = 'risk_report_config'
       and g.owning_component = 'ores.reporting'
       and g.name = 'Test exposure'
       and g.valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'the reporting configuration is registered under the risk report configuration type');

select is(
    (select s.portfolio_id from ores_reporting_risk_report_config_portfolios_tbl s, t_live r
     where s.tenant_id = ores_utility_system_tenant_id_fn()
       and s.risk_report_config_id = r.id
       and s.valid_to = ores_utility_infinity_timestamp_fn()),
    (select portfolio_id from t_ctx),
    'the scope is the portfolio the party holds under the staged name');

select results_eq(
    $$select a.analytic_type_code, a.display_order
      from ores_reporting_configuration_analytics_tbl a, t_live r
      where a.tenant_id = ores_utility_system_tenant_id_fn()
        and a.configuration_id = r.configuration_id
        and a.valid_to = ores_utility_infinity_timestamp_fn()
      order by a.display_order$$,
    $$values ('simulation'::text, 1), ('pfe'::text, 2)$$,
    'the analytics belong to the configuration, in order');

select results_eq(
    $$select d.subtype, d.name, p.value
      from ores_reporting_configuration_analytic_parameters_tbl p
      join ores_reporting_parameter_definitions_tbl d
        on d.tenant_id = ores_utility_system_tenant_id_fn()
       and d.id = p.parameter_definition_id
       and d.valid_to = ores_utility_infinity_timestamp_fn()
      join ores_reporting_configuration_analytics_tbl a
        on a.tenant_id = p.tenant_id and a.id = p.configuration_analytic_id
       and a.valid_to = ores_utility_infinity_timestamp_fn()
      join t_live r on r.configuration_id = a.configuration_id
      where p.tenant_id = ores_utility_system_tenant_id_fn()
        and p.valid_to = ores_utility_infinity_timestamp_fn()$$,
    $$values ('pfe'::text, 'quantile'::text, '0.95'::text)$$,
    'the parameter is resolved to the definition of its analytic type');

select results_eq(
    $$select * from pg_temp.publish()$$,
    $$values ('skipped'::text, 5::bigint)$$,
    'a repeat publish adds nothing');

select is(
    ores_reporting_configuration_analytics_problem_fn(
        ores_utility_system_tenant_id_fn(), (select configuration_id from t_live)),
    null,
    'the published analytics make a run');

-- A configuration with no analytic, and then one whose only analytic reads the output
-- of another that is absent.
insert into ores_reporting_configurations_tbl (
    tenant_id, id, version, party_id, name, configuration_type_code, owning_component,
    modified_by, performed_by, change_reason_code, change_commentary)
select ores_utility_system_tenant_id_fn(), c.lonely_configuration_id, 0, c.party_id,
    'Test lonely exposure', 'risk_report_config', 'ores.reporting',
    current_user, current_user, 'system.test', 'pgTAP fixture'
from t_ctx c;

select is(
    ores_reporting_configuration_analytics_problem_fn(
        ores_utility_system_tenant_id_fn(), (select lonely_configuration_id from t_ctx)),
    'The configuration switches no analytic on, and an ORE run needs one.',
    'a configuration with no analytic does not make a run');

insert into ores_reporting_configuration_analytics_tbl (
    tenant_id, id, version, party_id, configuration_id, analytic_type_code, display_order,
    active, modified_by, performed_by, change_reason_code, change_commentary)
select ores_utility_system_tenant_id_fn(), '00000000-0000-0000-0000-0000000e0009'::uuid, 0,
    c.party_id, c.lonely_configuration_id, 'pfe', 1, 'Y',
    current_user, current_user, 'system.test', 'pgTAP fixture'
from t_ctx c;

select is(
    ores_reporting_configuration_analytics_problem_fn(
        ores_utility_system_tenant_id_fn(), (select lonely_configuration_id from t_ctx)),
    'The pfe analytic needs the simulation analytic.',
    'an analytic that reads the output of another needs that analytic on');

update ores_dq_configuration_analytic_parameters_artefact_tbl
set parameter_name = 'noSuchParameter'
where dataset_id = (select id from t_dataset);

select throws_ok(
    $$select * from pg_temp.publish()$$,
    'P0001',
    null,
    'a parameter that no definition describes is refused');

update ores_dq_configuration_analytic_parameters_artefact_tbl
set parameter_name = 'quantile'
where dataset_id = (select id from t_dataset);

insert into ores_dq_risk_report_configs_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, name, description, configuration_id,
    base_currency, observation_model, n_threads, market_data_type, input_path, output_path,
    log_file, log_mask, imply_todays_fixings, scope_portfolio)
select d.id, ores_utility_system_tenant_id_fn(), c.empty_config_id, 0, c.party_id,
    'Test empty', 'A configuration that stages no analytic', gen_random_uuid(),
    'GBP', 'Disable', 1, 'eod', 'Input', 'Output', 'log.txt', null, null, null
from t_dataset d, t_ctx c;

select throws_ok(
    $$select * from pg_temp.publish()$$,
    'P0001',
    null,
    'a configuration that stages no analytic is refused');

select * from finish();
rollback;
