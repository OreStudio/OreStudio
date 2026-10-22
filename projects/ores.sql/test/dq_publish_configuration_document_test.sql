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
 * pgTAP tests for the guards of the configuration document publish.
 *
 * The publish re-keys every uuid column of a document, so a uuid column that refers
 * outside the document would point at nothing:
 * - The uuid key columns of the documents are the ones known to refer inside
 *   them, so a new uuid column is a decision and not an accident
 * - A column named as external is copied as staged
 * - A staged header with no configuration is refused
 *
 * Run with: pg_prove -d <database> test/dq_publish_configuration_document_test.sql
 */

begin;

select plan(4);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.visible_party_ids',
    (select '{' || string_agg(id::text, ',') || '}' from ores_refdata_parties_tbl), true);

-- Every uuid column of the document tables other than the key, the tenant, the party, the
-- reporting configuration and the ore parameter, which a publish resolves by name.
-- Each refers to a row of the same document: a header, a collection, a curve definition,
-- a segment or an analytic.
select is(
    (select string_agg(distinct column_name, ' ' order by column_name)
     from information_schema.columns c
     where table_schema = 'public' and data_type = 'uuid'
       and column_name not in ('id', 'tenant_id', 'party_id', 'configuration_id',
                               'ore_parameter_id')
       and table_name in (
         select concat('ores_analytics_', t, '_tbl') from unnest(array['pricing_model_configs',
           'pricing_model_products', 'pricing_model_product_parameters', 'todays_market_configs',
           'todays_market_collections', 'todays_market_entries', 'todays_market_configurations',
           'todays_market_configuration_bindings']) t
         union all
         select concat('ores_refdata_', t, '_tbl') from unnest(array['curve_configurations',
           'curve_configuration_sections', 'curve_global_reports', 'curve_definitions',
           'yield_curve_configs', 'equity_curve_configs', 'default_curve_configs',
           'default_curve_configurations', 'inflation_curve_configs',
           'inflation_seasonality_factors', 'curve_security_configs',
           'intraday_power_curve_configs', 'fx_volatility_configs', 'base_correlation_configs',
           'curve_correlation_configs', 'curve_report_configurations', 'cds_volatility_configs',
           'cds_volatility_terms', 'curve_volatility_configs',
           'inflation_cap_floor_volatility_configs', 'swaption_volatility_configs',
           'cap_floor_volatility_configs', 'curve_parametric_smiles',
           'curve_parametric_smile_parameters', 'equity_volatility_configs',
           'commodity_volatility_configs', 'bond_future_volatility_configs',
           'curve_bootstrap_configs', 'curve_segments', 'curve_segment_curves',
           'curve_quotes']) t
         union all
         select concat('ores_reporting_', t, '_tbl') from unnest(array['risk_report_configs',
           'configuration_analytics', 'configuration_analytic_parameters',
           'configuration_market_bindings']) t)),
    'commodity_price_segment_id configuration_analytic_id curve_configuration_id curve_definition_id curve_segment_id '
    'default_curve_configuration_id pricing_model_config_id pricing_model_product_id '
    'todays_market_collection_id todays_market_config_id todays_market_configuration_id',
    'every uuid key column of a document refers to a row of the same document');

create temp table t_ctx on commit drop as
select (select id from ores_refdata_parties_tbl
        where tenant_id = ores_utility_system_tenant_id_fn() and short_code = 'system_party'
          and valid_to = ores_utility_infinity_timestamp_fn()) as party_id,
       '00000000-0000-0000-0000-0000000a0001'::uuid as header_id,
       '00000000-0000-0000-0000-0000000a0002'::uuid as product_id,
       '00000000-0000-0000-0000-0000000a0003'::uuid as parameter_id,
       '00000000-0000-0000-0000-0000000a0004'::uuid as configuration_id,
       '00000000-0000-0000-0000-0000000a0005'::uuid as outside_id;

do $$
begin
    perform ores_dq_datasets_upsert_fn(ores_utility_system_tenant_id_fn(),
        'test.guarded_document', 'ACME Corporation', 'Reporting', 'Reference Data', 'NONE',
        'Primary', 'Synthetic', 'Raw', 'OreStudio Code Generation Methodology',
        'Test Guarded Document', 'A document staged for a test.',
        'ACMEBANK', 'pgTAP fixture', current_date, 'Internal Use Only', 'pricing_engines');
end $$;

create temp table t_dataset on commit drop as
select id from ores_dq_datasets_tbl
where code = 'test.guarded_document' and valid_to = ores_utility_infinity_timestamp_fn();

-- A header with no configuration.
insert into ores_dq_pricing_model_configs_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, name, description, config_variant,
    configuration_id)
select d.id, ores_utility_system_tenant_id_fn(), c.header_id, 0, c.party_id,
    'Test guarded document', 'A document', '', null
from t_dataset d, t_ctx c;

select throws_ok(
    $$select * from ores_analytics_publish_pricing_engines_from_dq_fn(
          (select id from t_dataset), ores_utility_system_tenant_id_fn(), 'upsert',
          jsonb_build_object('party_id', (select party_id::text from t_ctx)))$$,
    'P0001',
    null,
    'a header with no configuration is refused');

-- The header gets its configuration, and a parameter refers to a row outside the document.
update ores_dq_pricing_model_configs_artefact_tbl
set configuration_id = (select configuration_id from t_ctx)
where dataset_id = (select id from t_dataset);

insert into ores_dq_pricing_model_product_parameters_artefact_tbl (
    dataset_id, tenant_id, id, version, party_id, pricing_model_config_id,
    pricing_model_product_id, parameter_scope, parameter_name, parameter_value, position)
select d.id, ores_utility_system_tenant_id_fn(), c.parameter_id, 0, c.party_id, c.header_id,
    c.outside_id, 'global', 'ContinueOnCalibrationError', 'true', 0
from t_dataset d, t_ctx c;

select lives_ok(
    $$select * from ores_dq_publish_configuration_document_fn(
          (select id from t_dataset), ores_utility_system_tenant_id_fn(),
          (select party_id from t_ctx), 'pricing_engines', 'ores.analytics', 'ores_analytics_',
          array['pricing_model_configs', 'pricing_model_product_parameters'],
          array['pricing_model_product_id'])$$,
    'a document publishes when it names an external column');

select is(
    (select p.pricing_model_product_id
     from ores_analytics_pricing_model_product_parameters_tbl p, t_ctx c
     where p.tenant_id = ores_utility_system_tenant_id_fn() and p.party_id = c.party_id
       and p.parameter_name = 'ContinueOnCalibrationError'
       and p.valid_to = ores_utility_infinity_timestamp_fn()),
    '00000000-0000-0000-0000-0000000a0005'::uuid,
    'an external column is copied as staged and not re-keyed');

select * from finish();
rollback;
