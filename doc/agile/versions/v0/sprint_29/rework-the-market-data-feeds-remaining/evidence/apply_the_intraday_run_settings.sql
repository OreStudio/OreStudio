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
 * The intraday run settings and the sensitivity bindings applied to an
 * environment that is already built.
 *
 * The seeds moved under a running database, so this script stages the
 * regenerated ACME artefacts and publishes them in upsert mode. Two things
 * change and nothing is dropped.
 *
 * The close twins gain their own run settings rows. A definition states one cut
 * selection, and the selection is the market data the run prices from, so a
 * definition that selects the EOD cut binds a run settings row whose market data
 * type is eod. Until now the twins shared the live row, and an EOD report priced
 * its figures from the instant the run started.
 *
 * Every configuration that switches the sensitivity analytic on gains the two
 * files that analytic reads. A run whose configuration names no sensitivity
 * configuration asks the engine for sensitivities with no factors to shift, and
 * the engine declines.
 *
 * Run from the repository root, against the environment's own database:
 *
 *   ./compass.sh sql -- -v ON_ERROR_STOP=1 \
 *       -f doc/agile/versions/v0/sprint_29/rework-the-market-data-feeds-remaining/evidence/apply_the_intraday_run_settings.sql
 *
 * It is idempotent: a second run stages the same rows and publishes nothing new,
 * because the publish writes only where the staged statement differs from the
 * row the party holds.
 */

\set ON_ERROR_STOP on

\echo '--- The publish carries an upsert ---'

\i projects/ores.sql/create/reporting/reporting_publish_from_dq_create.sql

\echo '--- Stage the regenerated ACME artefacts ---'

\i projects/ores.sql/populate/acme/acme_group_report_definitions_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_uk_report_definitions_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_us_report_definitions_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_hk_report_definitions_artefact_populate.sql

\i projects/ores.sql/populate/acme/acme_group_risk_report_configs_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_uk_risk_report_configs_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_us_risk_report_configs_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_hk_risk_report_configs_artefact_populate.sql

\echo '--- Publish the run settings, so the close twins price from the close ---'

select 'group risk report configs' as dataset, *
from ores_reporting_publish_risk_report_configs_from_dq_fn(
    (select id from ores_dq_datasets_tbl
     where code = 'acme.acme_group.risk_report_configs'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select id from ores_iam_tenants_tbl where code = 'acme_corporation'
       and valid_to = ores_utility_infinity_timestamp_fn()), 'upsert',
    jsonb_build_object('party_id', (select id from ores_refdata_parties_tbl where short_code = 'ACMCOR'
         and tenant_id = (select id from ores_iam_tenants_tbl
                          where code = 'acme_corporation'
                            and valid_to = ores_utility_infinity_timestamp_fn())
         and valid_to = ores_utility_infinity_timestamp_fn())));

select 'uk risk report configs' as dataset, *
from ores_reporting_publish_risk_report_configs_from_dq_fn(
    (select id from ores_dq_datasets_tbl
     where code = 'acme.acme_uk.risk_report_configs'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select id from ores_iam_tenants_tbl where code = 'acme_corporation'
       and valid_to = ores_utility_infinity_timestamp_fn()), 'upsert',
    jsonb_build_object('party_id', (select id from ores_refdata_parties_tbl where short_code = 'ACCOUK'
         and tenant_id = (select id from ores_iam_tenants_tbl
                          where code = 'acme_corporation'
                            and valid_to = ores_utility_infinity_timestamp_fn())
         and valid_to = ores_utility_infinity_timestamp_fn())));

select 'us risk report configs' as dataset, *
from ores_reporting_publish_risk_report_configs_from_dq_fn(
    (select id from ores_dq_datasets_tbl
     where code = 'acme.acme_us.risk_report_configs'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select id from ores_iam_tenants_tbl where code = 'acme_corporation'
       and valid_to = ores_utility_infinity_timestamp_fn()), 'upsert',
    jsonb_build_object('party_id', (select id from ores_refdata_parties_tbl where short_code = 'ACCOUS'
         and tenant_id = (select id from ores_iam_tenants_tbl
                          where code = 'acme_corporation'
                            and valid_to = ores_utility_infinity_timestamp_fn())
         and valid_to = ores_utility_infinity_timestamp_fn())));

select 'hk risk report configs' as dataset, *
from ores_reporting_publish_risk_report_configs_from_dq_fn(
    (select id from ores_dq_datasets_tbl
     where code = 'acme.acme_hk.risk_report_configs'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select id from ores_iam_tenants_tbl where code = 'acme_corporation'
       and valid_to = ores_utility_infinity_timestamp_fn()), 'upsert',
    jsonb_build_object('party_id', (select id from ores_refdata_parties_tbl where short_code = 'ACCOHK'
         and tenant_id = (select id from ores_iam_tenants_tbl
                          where code = 'acme_corporation'
                            and valid_to = ores_utility_infinity_timestamp_fn())
         and valid_to = ores_utility_infinity_timestamp_fn())));

\echo '--- Publish the definitions, so each one binds its own run settings ---'

select 'group definitions' as dataset, *
from ores_reporting_publish_report_definitions_from_dq_fn(
    (select id from ores_dq_datasets_tbl
     where code = 'acme.acme_group.report_definitions'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select id from ores_iam_tenants_tbl where code = 'acme_corporation'
       and valid_to = ores_utility_infinity_timestamp_fn()), 'upsert',
    jsonb_build_object('party_id', (select id from ores_refdata_parties_tbl where short_code = 'ACMCOR'
         and tenant_id = (select id from ores_iam_tenants_tbl
                          where code = 'acme_corporation'
                            and valid_to = ores_utility_infinity_timestamp_fn())
         and valid_to = ores_utility_infinity_timestamp_fn())));

select 'uk definitions' as dataset, *
from ores_reporting_publish_report_definitions_from_dq_fn(
    (select id from ores_dq_datasets_tbl
     where code = 'acme.acme_uk.report_definitions'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select id from ores_iam_tenants_tbl where code = 'acme_corporation'
       and valid_to = ores_utility_infinity_timestamp_fn()), 'upsert',
    jsonb_build_object('party_id', (select id from ores_refdata_parties_tbl where short_code = 'ACCOUK'
         and tenant_id = (select id from ores_iam_tenants_tbl
                          where code = 'acme_corporation'
                            and valid_to = ores_utility_infinity_timestamp_fn())
         and valid_to = ores_utility_infinity_timestamp_fn())));

select 'us definitions' as dataset, *
from ores_reporting_publish_report_definitions_from_dq_fn(
    (select id from ores_dq_datasets_tbl
     where code = 'acme.acme_us.report_definitions'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select id from ores_iam_tenants_tbl where code = 'acme_corporation'
       and valid_to = ores_utility_infinity_timestamp_fn()), 'upsert',
    jsonb_build_object('party_id', (select id from ores_refdata_parties_tbl where short_code = 'ACCOUS'
         and tenant_id = (select id from ores_iam_tenants_tbl
                          where code = 'acme_corporation'
                            and valid_to = ores_utility_infinity_timestamp_fn())
         and valid_to = ores_utility_infinity_timestamp_fn())));

select 'hk definitions' as dataset, *
from ores_reporting_publish_report_definitions_from_dq_fn(
    (select id from ores_dq_datasets_tbl
     where code = 'acme.acme_hk.report_definitions'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select id from ores_iam_tenants_tbl where code = 'acme_corporation'
       and valid_to = ores_utility_infinity_timestamp_fn()), 'upsert',
    jsonb_build_object('party_id', (select id from ores_refdata_parties_tbl where short_code = 'ACCOHK'
         and tenant_id = (select id from ores_iam_tenants_tbl
                          where code = 'acme_corporation'
                            and valid_to = ores_utility_infinity_timestamp_fn())
         and valid_to = ores_utility_infinity_timestamp_fn())));

\echo '--- What each party now holds for its intraday reports ---'

select p.short_code as party, d.name, d.cut_kind_code, cfg.name as run_settings,
       cfg.market_data_type, pe.name as pricing_engines
from ores_reporting_report_definitions_tbl d
join ores_refdata_parties_tbl p
  on p.id = d.party_id and p.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_reporting_report_configurations_tbl b
  on b.report_definition_id = d.id
 and b.configuration_type_code = 'risk_report_config'
 and b.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_reporting_configurations_tbl h
  on h.id = b.configuration_id and h.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_reporting_risk_report_configs_tbl cfg
  on cfg.configuration_id = h.id and cfg.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_reporting_report_configurations_tbl bpe
  on bpe.report_definition_id = d.id
 and bpe.configuration_type_code = 'pricing_engines'
 and bpe.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_reporting_configurations_tbl hpe
  on hpe.id = bpe.configuration_id and hpe.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_analytics_pricing_model_configs_tbl pe
  on pe.configuration_id = hpe.id and pe.valid_to = ores_utility_infinity_timestamp_fn()
where d.valid_to = ores_utility_infinity_timestamp_fn()
  and d.name in ('Intraday Risk Monitor', 'Headline Position', 'Headline Position (EOD)',
                 'Spot Greeks', 'Spot Greeks (EOD)', 'Triple Vol Spot Ladder',
                 'Triple Vol Spot Ladder (EOD)', 'Spot Ladder', 'Spike Map', 'Strike Map',
                 'Option Map', 'Intraday Change', 'Trades Since Last In')
order by d.name, p.short_code;

\echo '--- The two files every sensitivity configuration names ---'

select cfg.name as run_settings,
       max(case when ap.name = 'sensitivityConfigFile' then v.value end) as sensitivity_config,
       max(case when ap.name = 'marketConfigFile' then v.value end) as market_config
from ores_reporting_risk_report_configs_tbl cfg
join ores_reporting_configuration_analytics_tbl a
  on a.configuration_id = cfg.configuration_id
 and a.analytic_type_code = 'sensitivity'
 and a.valid_to = ores_utility_infinity_timestamp_fn()
join ores_reporting_configuration_analytic_parameters_tbl v
  on v.configuration_analytic_id = a.id and v.valid_to = ores_utility_infinity_timestamp_fn()
join ores_reporting_ore_parameters_tbl ap
  on ap.id = v.ore_parameter_id and ap.valid_to = ores_utility_infinity_timestamp_fn()
where cfg.valid_to = ores_utility_infinity_timestamp_fn()
group by cfg.name
order by cfg.name;
