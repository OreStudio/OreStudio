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
 * The intraday selections applied to an environment that is already built.
 *
 * A development environment is provisioned once and stays up while the seeds
 * move under it, so a seed change reaches a running database by being applied
 * rather than by the database being built again. This script does that for the
 * two seeds this task moved: the artefact table gains the column a catalogue
 * states a report's cut selection in, the regenerated artefact populates are
 * staged again, and the publish carries the staged statement onto the rows the
 * parties already hold. Nothing is dropped and nothing is recreated: the
 * definitions keep their identities, so their runs, bindings and schedules
 * still name them.
 *
 * Run from the repository root, against the environment's own database:
 *
 *   ./compass.sh sql -- -v ON_ERROR_STOP=1 \
 *       -f doc/agile/versions/v0/sprint_29/rework-the-market-data-feeds-remaining/evidence/apply_the_intraday_selections.sql
 *
 * It is idempotent: a second run stages the same rows and publishes nothing
 * new, because the publish writes only where the staged statement differs from
 * the row the party holds.
 */

\set ON_ERROR_STOP on

\echo '--- The artefact carries the cut selection ---'

-- Additive: the column is new, nullable, and states nothing for a report the
-- catalogue does not classify by cycle.
alter table ores_dq_report_definitions_artefact_tbl
    add column if not exists cut_kind_code text;

-- The definitions this environment holds were published by a build that
-- predates one column the checkout reads: the definition states which result
-- families it skips, and a reader of a definition selects it. The column is
-- additive, so the environment gains it here rather than by being built again.
alter table ores_reporting_report_definitions_tbl
    add column if not exists skipped_families text not null default '';

\echo '--- The publish carries it, and honours an upsert ---'

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

\echo '--- Publish the run settings, so the configurations say the live market data ---'

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

\echo '--- Publish the definitions, so each report states the cut it prices from ---'

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

\echo '--- What each party now holds for its intraday reports ---'

-- A definition binds a reporting configuration, and a risk report configuration
-- names that same configuration as its header, so the run settings are two hops
-- from the definition: definition -> binding -> header -> settings.
select p.short_code as party, d.name, d.cut_kind_code, cfg.market_data_type
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
  and d.cut_kind_code is not null
order by p.short_code, d.cut_kind_code, d.name;
