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
 * The ladder family applied to an environment that is already built.
 *
 * A development environment is provisioned once and stays up while the seeds
 * move under it, so a seed change reaches a running database by being applied
 * rather than by the database being built again. This script does that for the
 * fifteen ladder definitions and the run settings they bind: the regenerated
 * artefact populates are staged again, and the publish carries them onto the
 * rows the parties already hold. Nothing is dropped and nothing is recreated:
 * a definition the environment already holds keeps its identity, so its runs,
 * bindings and schedules still name it.
 *
 * The Northwind artefacts are staged and not published. This environment holds
 * no Northwind tenant, so there is no party for the publish to write to; the
 * rows wait in the dataset for the provision that creates the tenant.
 *
 * Run from the repository root, against the environment's own database:
 *
 *   ./compass.sh sql -- -v ON_ERROR_STOP=1 \
 *       -f doc/agile/versions/v0/sprint_29/rework-the-market-data-feeds-remaining/evidence/apply_the_ladder_family.sql
 *
 * It is idempotent: a second run stages the same rows and publishes nothing
 * new, because the publish writes only where the staged statement differs from
 * the row the party holds.
 */

\set ON_ERROR_STOP on

\echo '--- Stage the run settings first, so the definitions can bind them ---'

\i projects/ores.sql/populate/acme/acme_group_risk_report_configs_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_uk_risk_report_configs_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_us_risk_report_configs_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_hk_risk_report_configs_artefact_populate.sql

\echo '--- Stage the definitions and their bindings ---'

\i projects/ores.sql/populate/acme/acme_group_report_definitions_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_uk_report_definitions_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_us_report_definitions_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_hk_report_definitions_artefact_populate.sql

\echo '--- Stage Northwind, which waits for its tenant ---'

\i projects/ores.sql/populate/northwind/northwind_report_definitions_artefact_populate.sql
\i projects/ores.sql/populate/northwind/northwind_risk_report_configs_artefact_populate.sql

\echo '--- Publish the run settings the ladders bind ---'

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

\echo '--- Publish the definitions, so each ladder states its cut and its bindings ---'

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

\echo '--- The ladder family each party now holds, with its cut and its run settings ---'

-- A definition binds a reporting configuration, and a risk report configuration
-- names that same configuration as its header, so the run settings are two hops
-- from the definition: definition -> binding -> header -> settings.
select p.short_code as party, d.name, d.cut_kind_code,
       d.schedule_expression as schedule, cfg.name as run_settings,
       cfg.market_data_type as market_data
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
where d.valid_to = ores_utility_infinity_timestamp_fn()
  and d.name like 'Bump and Reprice % Ladder%'
order by p.short_code, d.name;

\echo '--- The two sensitivity files each ladder analytic reads ---'

select p.short_code as party, cfg.name as run_settings, a.analytic_type_code,
       op.name as parameter, v.value
from ores_reporting_risk_report_configs_tbl cfg
join ores_refdata_parties_tbl p
  on p.id = cfg.party_id and p.valid_to = ores_utility_infinity_timestamp_fn()
join ores_reporting_configuration_analytics_tbl a
  on a.configuration_id = cfg.configuration_id and a.valid_to = ores_utility_infinity_timestamp_fn()
 and a.analytic_type_code = 'sensitivity'
join ores_reporting_configuration_analytic_parameters_tbl v
  on v.configuration_analytic_id = a.id and v.valid_to = ores_utility_infinity_timestamp_fn()
join ores_reporting_ore_parameters_tbl op
  on op.id = v.ore_parameter_id
 and op.name in ('sensitivityConfigFile', 'marketConfigFile')
where cfg.valid_to = ores_utility_infinity_timestamp_fn()
  and cfg.name like '%Sensitivities%'
order by p.short_code, cfg.name, op.name;
