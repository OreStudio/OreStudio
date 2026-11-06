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
 * The intraday cadences applied to an environment that is already built.
 *
 * A development environment is provisioned once and stays up while the seeds
 * move under it, so a seed change reaches a running database by being applied
 * rather than by the database being built again. This script stages the four
 * ACME definition datasets and the Northwind one, publishes the four parties,
 * and prints what each party now holds.
 *
 * The publish carries the changed statement onto the row the party already
 * holds, as a new version of it. It keeps the definition's identity, its run
 * grant and its scheduler job, so a definition that was scheduled before this
 * script runs still names a job. That job carries its own copy of the
 * expression, so the schedule is moved by the scheduling operations in the
 * shell and not by this script: see the task's evidence file.
 *
 * The Northwind artefacts are staged and not published. This environment holds
 * no Northwind tenant, so there is no party for the publish to write to.
 *
 * Run from the repository root, against the environment's own database:
 *
 *   ./compass.sh sql -- -v ON_ERROR_STOP=1 \
 *       -f doc/agile/versions/v0/sprint_29/rework-the-market-data-feeds-remaining/evidence/apply_the_intraday_frequencies.sql
 *
 * It is idempotent: a second run stages the same rows and publishes nothing,
 * because the publish writes only where the staged statement differs from the
 * row the party holds.
 */

\set ON_ERROR_STOP on

\echo '--- Stage the four ACME definition datasets ---'

\i projects/ores.sql/populate/acme/acme_group_report_definitions_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_uk_report_definitions_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_us_report_definitions_artefact_populate.sql
\i projects/ores.sql/populate/acme/acme_hk_report_definitions_artefact_populate.sql

\echo '--- Stage Northwind, which waits for its tenant ---'

\i projects/ores.sql/populate/northwind/northwind_report_definitions_artefact_populate.sql

\echo '--- Publish the definitions of each party ---'

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

\echo '--- What each party states after the publish ---'

select p.short_code as party, d.id, d.name, d.schedule_expression as schedule,
       d.cut_kind_code as cut,
       coalesce(j.schedule_expression, '(no job)') as job_schedule,
       coalesce(j.is_active::text, '-') as job_active
from ores_reporting_report_definitions_tbl d
join ores_refdata_parties_tbl p
  on p.id = d.party_id and p.valid_to = ores_utility_infinity_timestamp_fn()
left join ores_scheduler_job_definitions_tbl j
  on j.id = d.scheduler_job_id and j.valid_to = ores_utility_infinity_timestamp_fn()
where d.valid_to = ores_utility_infinity_timestamp_fn()
  and d.name in ('Triple Vol Spot Ladder', 'Triple Vol Spot Ladder (EOD)',
                 'Bump and Reprice Surface Vega Ladder',
                 'Bump and Reprice Surface Vega Ladder (EOD)',
                 'Spot Ladder', 'Trades Since Last In')
order by p.short_code, d.name;
