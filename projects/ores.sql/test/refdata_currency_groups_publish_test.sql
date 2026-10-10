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
 * pgTAP tests for the currency groups and their members.
 *
 * Tests cover the seeded datasets and the base bundle that provisions a
 * tenant:
 * - the base bundle holds both datasets, and the members need the groups
 * - the artefact tables hold the six groups and 37 memberships
 * - a publish into a tenant writes every group and every member
 *
 * Run with: pg_prove -d ores_dev_local1 test/refdata_currency_groups_publish_test.sql
 */

begin;

select plan(10);

select is(
    (select count(*)::int from ores_dq_dataset_bundle_members_tbl
     where bundle_code = 'base'
       and dataset_code in ('refdata.currency_groups', 'refdata.currency_currency_groups')
       and valid_to = ores_utility_infinity_timestamp_fn()),
    2,
    'base bundle: holds the groups and the members datasets'
);

select is(
    (select count(*)::int from ores_dq_dataset_dependencies_tbl
     where dataset_code = 'refdata.currency_currency_groups'
       and dependency_code = 'refdata.currency_groups'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1,
    'dependencies: the members dataset needs the groups dataset'
);

select is(
    (select target_subject from ores_dq_artefact_types_tbl
     where code = 'currency_groups' and valid_to = ores_utility_infinity_timestamp_fn()),
    'refdata.v1.ops.publish_currency_groups_from_dq',
    'artefact type: currency groups publish through their own subject'
);

select is(
    (select target_subject from ores_dq_artefact_types_tbl
     where code = 'currency_currency_groups' and valid_to = ores_utility_infinity_timestamp_fn()),
    'refdata.v1.ops.publish_currency_currency_groups_from_dq',
    'artefact type: group members publish through their own subject'
);

select is(
    (select count(*)::int from ores_dq_currency_groups_artefact_tbl),
    6,
    'groups artefact: holds the six desk groups'
);

select is(
    (select count(*)::int from ores_dq_currency_currency_groups_artefact_tbl),
    37,
    'members artefact: holds 37 memberships'
);

select is(
    (select count(*)::int from ores_dq_currency_currency_groups_artefact_tbl m
     where not exists (
         select 1 from ores_dq_currency_groups_artefact_tbl g
         where g.code = m.currency_group_code)),
    0,
    'members artefact: every member names a seeded group'
);

select results_eq(
    $$select sum(record_count)::int from ores_refdata_publish_currency_groups_from_dq_fn(
        (select id from ores_dq_datasets_tbl
         where code = 'refdata.currency_groups'
           and valid_to = ores_utility_infinity_timestamp_fn()),
        ores_utility_system_tenant_id_fn())
      where action in ('inserted', 'updated')$$,
    $$values (6)$$,
    'publish: the groups dataset writes six groups'
);

select results_eq(
    $$select sum(record_count)::int from ores_refdata_publish_currency_currency_groups_from_dq_fn(
        (select id from ores_dq_datasets_tbl
         where code = 'refdata.currency_currency_groups'
           and valid_to = ores_utility_infinity_timestamp_fn()),
        ores_utility_system_tenant_id_fn())
      where action in ('inserted', 'updated')$$,
    $$values (37)$$,
    'publish: the members dataset writes 37 memberships'
);

select is(
    (select array_agg(currency_group_code order by currency_group_code)::text
     from ores_refdata_currency_currency_groups_tbl
     where currency_iso_code = 'NOK'
       and tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    '{COMMODITY,G11,SCANDIES}',
    'publish: NOK sits in G11, Scandies and the commodity currencies'
);

select * from finish();

rollback;
