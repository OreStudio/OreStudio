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
 * pgTAP tests for the tenant bootstrap.
 *
 * A new tenant is seeded with the bundles the bundle map makes due when a tenant
 * is created, and with nothing else. The suite provisions a tenant and asserts
 * three things about it:
 *
 * - The tenant holds the rows the tenant_defaults bundle publishes, in the
 *   number the staged artefacts state, and every row names the dataset that
 *   produced it.
 * - The tenant holds no copy of the platform's catalogues: the DQ registries,
 *   the trading lookups, the series classification rules, the party categories.
 *   It still reads them, because the one copy stays in the system tenant.
 * - A bundle resolves for the tenant even though the dataset rows are the system
 *   tenant's, which is what lets a tenant publish a bundle it does not own the
 *   staging for.
 *
 * The last test states what the second call of the bootstrap costs: the publish
 * functions rewrite their rows rather than skipping them, so the run reports
 * updates and inserts nothing. That is the guarantee the bootstrap leans on.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/dq_tenant_bootstrap_test.sql
 */

begin;

select plan(9);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

-- Read under the system tenant: a tenant's session cannot see the staged
-- artefacts, and the expected counts are exactly what the artefacts state.
create temporary table t_expected on commit drop as
select
    (select count(*) from ores_dq_badge_severities_artefact_tbl a
     where a.dataset_id = (select id from ores_dq_datasets_tbl
                           where code = 'ore.badge_severities'
                             and tenant_id = ores_utility_system_tenant_id_fn()
                             and valid_to = ores_utility_infinity_timestamp_fn())) as severities,
    (select count(*) from ores_dq_badge_definitions_artefact_tbl a
     where a.dataset_id = (select id from ores_dq_datasets_tbl
                           where code = 'ore.badge_definitions'
                             and tenant_id = ores_utility_system_tenant_id_fn()
                             and valid_to = ores_utility_infinity_timestamp_fn())) as definitions,
    (select count(*) from ores_dq_badge_mappings_artefact_tbl a
     where a.dataset_id = (select id from ores_dq_datasets_tbl
                           where code = 'ore.badge_mappings'
                             and tenant_id = ores_utility_system_tenant_id_fn()
                             and valid_to = ores_utility_infinity_timestamp_fn())) as mappings,
    (select count(*) from ores_dq_code_domains_artefact_tbl a
     where a.dataset_id = (select id from ores_dq_datasets_tbl
                           where code = 'ore.code_domains'
                             and tenant_id = ores_utility_system_tenant_id_fn()
                             and valid_to = ores_utility_infinity_timestamp_fn())) as domains,
    (select count(*) from ores_dq_artefact_types_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()) as artefact_types;

create temporary table t_tenant on commit drop as
select tenant_id as id from ores_iam_provision_tenant_fn(
    'production', 'test_tenant_bootstrap', 'Tenant Bootstrap Fixture',
    'test_tenant_bootstrap');

select set_config('app.current_tenant_id', (select id::text from t_tenant), true);

select is(
    (select count(*) from ores_dq_badge_severities_tbl
     where tenant_id = (select id from t_tenant)
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select severities from t_expected),
    'a new tenant holds the badge severities its bundle publishes');

select is(
    (select count(*) from ores_dq_badge_definitions_tbl
     where tenant_id = (select id from t_tenant)
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select definitions from t_expected),
    'a new tenant holds the badge definitions its bundle publishes');

select is(
    (select count(*) from ores_dq_badge_mappings_tbl
     where tenant_id = (select id from t_tenant)
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select mappings from t_expected),
    'a new tenant holds the badge mappings its bundle publishes');

select is(
    (select count(*) from ores_dq_code_domains_tbl
     where tenant_id = (select id from t_tenant)
       and valid_to = ores_utility_infinity_timestamp_fn()),
    (select domains from t_expected),
    'a new tenant holds the code domains its bundle publishes');

select is(
    (select count(*) from (
        select change_commentary from ores_dq_badge_severities_tbl
        where tenant_id = (select id from t_tenant)
          and valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select change_commentary from ores_dq_badge_definitions_tbl
        where tenant_id = (select id from t_tenant)
          and valid_to = ores_utility_infinity_timestamp_fn()) published
     where published.change_commentary not like 'Imported from DQ dataset: %'),
    0::bigint,
    'every seeded row names the dataset that produced it');

select is(
    (select count(*) from (
        select tenant_id from ores_dq_artefact_types_tbl
        where tenant_id = (select id from t_tenant)
        union all
        select tenant_id from ores_dq_dataset_bundles_tbl
        where tenant_id = (select id from t_tenant)
        union all
        select tenant_id from ores_dq_change_reasons_tbl
        where tenant_id = (select id from t_tenant)
        union all
        select tenant_id from ores_dq_change_reason_categories_tbl
        where tenant_id = (select id from t_tenant)
        union all
        select tenant_id from ores_dq_coding_schemes_tbl
        where tenant_id = (select id from t_tenant)
        union all
        select tenant_id from ores_dq_methodologies_tbl
        where tenant_id = (select id from t_tenant)
        union all
        select tenant_id from ores_dq_subject_areas_tbl
        where tenant_id = (select id from t_tenant)
        union all
        select tenant_id from ores_dq_data_domains_tbl
        where tenant_id = (select id from t_tenant)
        union all
        select tenant_id from ores_trading_activity_types_tbl
        where tenant_id = (select id from t_tenant)
        union all
        select tenant_id from ores_trading_structure_kinds_tbl
        where tenant_id = (select id from t_tenant)
        union all
        select tenant_id from ores_marketdata_series_classification_rules_tbl
        where tenant_id = (select id from t_tenant)
        union all
        select tenant_id from ores_refdata_party_categories_tbl
        where tenant_id = (select id from t_tenant)) copies),
    0::bigint,
    'a new tenant holds no copy of the catalogues the platform owns');

select is(
    (select count(*) from ores_dq_artefact_types_tbl
     where valid_to = ores_utility_infinity_timestamp_fn()),
    (select artefact_types from t_expected),
    'a tenant reads the artefact types where the system tenant holds them');

select is(
    (select count(*) from ores_dq_bundle_datasets_list_fn('world') where not has_artefacts),
    0::bigint,
    'every member of a bundle resolves for a tenant that holds no dataset row');

select is(
    (select coalesce(sum(record_count), 0)::bigint from ores_dq_tenant_bootstrap_fn(
        (select id from t_tenant)) where action = 'inserted'),
    0::bigint,
    'a second bootstrap inserts nothing, because the publish functions rewrite their rows');

select * from finish();

rollback;
