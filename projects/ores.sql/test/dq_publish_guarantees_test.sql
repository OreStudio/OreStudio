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
 * pgTAP tests for the guarantees a bundle publish states.
 *
 * A bundle is a run and not a transaction, so it states no all-or-nothing
 * promise and keeps no run log. The facts asserted here are:
 *
 * - no table holds a publication history and no function reads one;
 * - every bundle states a level from the closed set, and the level names the
 *   tenant the bundle writes into;
 * - a fixed platform vocabulary carries no artefact type, so nothing can
 *   publish a second copy of it into a tenant;
 * - the calendars declare no dependency on the coding schemes, which would
 *   republish them into every tenant.
 *
 * Run with: pg_prove -d <database> test/dq_publish_guarantees_test.sql
 */

begin;

select plan(7);

select is(
    to_regclass('public.ores_dq_bundle_publications_tbl')::text,
    null::text,
    'no table holds a publication history');

select is(
    to_regprocedure('public.ores_dq_get_bundle_publication_history_fn(text, integer)')::text,
    null::text,
    'no function reads a publication history');

select is(
    (select count(*) from ores_dq_dataset_bundles_tbl
     where valid_to = ores_utility_infinity_timestamp_fn()
       and level not in ('platform', 'world', 'tenant')),
    0::bigint,
    'every bundle states a level from the closed set');

select results_eq(
    $$select code, level from ores_dq_dataset_bundles_tbl
      where valid_to = ores_utility_infinity_timestamp_fn()
        and code in ('system_core', 'world', 'tenant_defaults')
      order by code$$,
    $$values ('system_core', 'platform'), ('tenant_defaults', 'tenant'), ('world', 'world')$$,
    'the three bundles with one level each state the level the classification assigns');

select is(
    (select count(*) from ores_dq_artefact_types_tbl
     where code = 'payment_frequencies'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    0::bigint,
    'the payment frequencies carry no artefact type, so no dataset can publish them');

select is(
    (select count(*) from ores_dq_dataset_dependencies_tbl
     where dataset_code = 'refdata.calendars'
       and dependency_code = 'iso.coding_schemes'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    0::bigint,
    'the calendars declare no dependency on the coding schemes');

select is(
    (select count(*) from ores_dq_dataset_dependencies_tbl
     where dataset_code = 'refdata.calendars'
       and dependency_code = 'iso.countries'
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'the calendars still order after the countries they validate against');

select * from finish();

rollback;
