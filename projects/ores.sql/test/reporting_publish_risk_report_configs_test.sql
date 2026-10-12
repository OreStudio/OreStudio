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
 * reads. The publish writes it for the party a publish names:
 * - The configuration and the reporting configuration it is registered as receive
 *   their rows, and the ids are re-keyed for the party
 * - The portfolio scope names the portfolio the party holds under that name
 * - A repeat publish adds nothing
 * - A scope that names a portfolio the party does not hold is refused
 * - A configuration that switches no analytic on is refused, because an engine run
 *   needs one
 * - A configuration that switches exposure profiles on without a simulation is
 *   refused, because the profiles are read from the cube a simulation writes
 *
 * Run with: pg_prove -d <database> test/reporting_publish_risk_report_configs_test.sql
 */

begin;

select plan(11);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       '00000000-0000-0000-0000-0000000e0001'::uuid as config_id,
       '00000000-0000-0000-0000-0000000e0002'::uuid as configuration_id,
       '00000000-0000-0000-0000-0000000e0003'::uuid as portfolio_id;

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
    base_currency, observation_model, n_threads, market_data_type,
    npv_enabled, cashflow_enabled, curves_enabled, sensitivity_enabled, simulation_enabled,
    xva_enabled, stress_enabled, parametric_var_enabled, initial_margin_enabled, pfe_enabled,
    portfolio_details_enabled, saccr_enabled, calibration_enabled, credit_curves_enabled,
    historical_var_enabled, pnl_explain_enabled, xva_cva_enabled, xva_dva_enabled, xva_fva_enabled, xva_colva_enabled, xva_dim_enabled,
    scope_portfolio)
select d.id, ores_utility_system_tenant_id_fn(), c.config_id, 0, c.party_id,
    'Test valuation', 'A risk report configuration', c.configuration_id,
    'GBP', 'disable', 1, 'eod',
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    'RRC-FIXTURE-PORTFOLIO'
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
    $$values ('inserted'::text, 1::bigint), ('scoped'::text, 1::bigint)$$,
    'the publish adds the configuration and its scope');

create temp view t_live as
select r.*
from ores_reporting_risk_report_configs_tbl r, t_ctx c
where r.tenant_id = ores_utility_system_tenant_id_fn() and r.party_id = c.party_id
  and r.name = 'Test valuation' and r.valid_to = ores_utility_infinity_timestamp_fn();

select is(
    (select r.id from t_live r),
    ores_dq_party_uuid_fn((select party_id from t_ctx), (select config_id from t_ctx)),
    'the configuration is re-keyed for the party');

select is(
    (select r.base_currency from t_live r),
    'GBP',
    'the configuration keeps its settings');

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
       and g.name = 'Test valuation'
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
    $$select * from pg_temp.publish()$$,
    $$values ('skipped'::text, 1::bigint)$$,
    'a repeat publish adds nothing');

select is(
    (select count(*) from ores_reporting_risk_report_config_portfolios_tbl s, t_live r
     where s.tenant_id = ores_utility_system_tenant_id_fn()
       and s.risk_report_config_id = r.id
       and s.valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'a repeat publish leaves one scope row');

update ores_dq_risk_report_configs_artefact_tbl
set scope_portfolio = 'NO-SUCH-PORTFOLIO'
where dataset_id = (select id from t_dataset);

select throws_ok(
    $$select * from pg_temp.publish()$$,
    'P0001',
    null,
    'a scope that names a portfolio the party does not hold is refused');

select throws_ok(
    $$insert into ores_reporting_risk_report_configs_tbl (
        id, tenant_id, version, party_id, name, configuration_id, base_currency,
        npv_enabled, cashflow_enabled,
        modified_by, performed_by, change_reason_code, change_commentary)
      select '00000000-0000-0000-0000-0000000e0009'::uuid, ores_utility_system_tenant_id_fn(), 0,
          party_id, 'Test nothing', configuration_id, 'GBP',
          0, 0, current_user, current_user, 'system.test', 'pgTAP fixture'
      from t_live$$,
    '23514',
    null,
    'a configuration that switches no analytic on is refused');

select throws_ok(
    $$insert into ores_reporting_risk_report_configs_tbl (
        id, tenant_id, version, party_id, name, configuration_id, base_currency,
        pfe_enabled, simulation_enabled,
        modified_by, performed_by, change_reason_code, change_commentary)
      select '00000000-0000-0000-0000-0000000e0007'::uuid, ores_utility_system_tenant_id_fn(), 0,
          party_id, 'Test profiles', configuration_id, 'GBP',
          1, 0, current_user, current_user, 'system.test', 'pgTAP fixture'
      from t_live$$,
    '23514',
    null,
    'exposure profiles without a simulation are refused');

select * from finish();
rollback;
