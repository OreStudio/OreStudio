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
 * pgTAP tests for the world catalogue, held once and read by every tenant.
 *
 * The world level is the reference data the platform adopts from an outside
 * standard: the ISO countries and currencies, the calendars and their
 * exceptions, the currency pairs and their conventions, the FpML vocabularies,
 * the commodities and the equity underlyings. It belongs to no tenant, so it
 * lives in the system tenant and a tenant reads it there.
 *
 * The facts asserted are:
 *
 * - the system tenant holds the world catalogue, from the bring-up publish;
 * - a tenant a person provisions holds no row of its own in any of six world
 *   catalogues, so no second copy exists;
 * - the same tenant reads the system tenant's calendars and currencies;
 * - it sees one row for a currency, not two, so the shared read does not
 *   double a fact the tenant does not own;
 * - it cannot move or close a shared calendar;
 * - no seed profile publishes the world bundle into its tenant, which is the
 *   mechanism that would recreate the copy.
 *
 * The tenant is one people set up, so the suite provisions it and lets the
 * transaction roll it back.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/refdata_world_catalogue_read_test.sql
 */

begin;

select plan(12);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is(
    (select count(*) from ores_refdata_calendars_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()) > 0,
    true,
    'the system tenant holds the world catalogue from the bring-up publish');

create temporary table t_tenant on commit drop as
select tenant_id as id from ores_iam_provision_tenant_fn(
    'production', 'test_world_catalogue', 'World Catalogue Fixture',
    'test_world_catalogue');

select set_config('app.current_tenant_id', (select id::text from t_tenant), true);

select is(
    (select count(*) from ores_refdata_calendars_tbl
     where tenant_id = ores_iam_current_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    0::bigint,
    'a provisioned tenant holds no calendar of its own');

select is(
    (select count(*) from ores_refdata_currencies_tbl
     where tenant_id = ores_iam_current_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    0::bigint,
    'a provisioned tenant holds no currency of its own');

select is(
    (select count(*) from ores_refdata_countries_tbl
     where tenant_id = ores_iam_current_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    0::bigint,
    'a provisioned tenant holds no country of its own');

select is(
    (select count(*) from ores_refdata_commodities_tbl
     where tenant_id = ores_iam_current_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn())
    + (select count(*) from ores_refdata_equity_underlyings_tbl
       where tenant_id = ores_iam_current_tenant_id_fn()
         and valid_to = ores_utility_infinity_timestamp_fn()),
    0::bigint,
    'a provisioned tenant holds no commodity and no equity underlying of its own');

select is(
    (select count(*) from ores_refdata_currency_pairs_tbl
     where tenant_id = ores_iam_current_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn())
    + (select count(*) from ores_refdata_currency_pair_conventions_tbl
       where tenant_id = ores_iam_current_tenant_id_fn()
         and valid_to = ores_utility_infinity_timestamp_fn()),
    0::bigint,
    'a provisioned tenant holds no currency pair and no pair convention of its own');

select is(
    (select count(*) from ores_refdata_calendars_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()) > 0,
    true,
    'a tenant reads the calendars the system tenant holds');

select is(
    (select count(*) from ores_refdata_currencies_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()) > 0,
    true,
    'a tenant reads the currencies the system tenant holds');

select is(
    (select max(copies) from (
        select count(*) as copies from ores_refdata_currencies_tbl
        where valid_to = ores_utility_infinity_timestamp_fn()
        group by iso_code) c),
    1::bigint,
    'a tenant sees one row for each currency, not a shared copy beside its own');

create temporary table t_calendar on commit drop as
select code from ores_refdata_calendars_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
order by code
limit 1;

create temporary table t_moved on commit drop as
with moved as (
    update ores_refdata_calendars_tbl
    set tenant_id = ores_iam_current_tenant_id_fn()
    where tenant_id = ores_utility_system_tenant_id_fn()
      and code = (select code from t_calendar)
    returning 1)
select count(*)::int as n from moved;

select is(
    (select n from t_moved),
    0,
    'a tenant moves no shared calendar into its own tenant');

delete from ores_refdata_calendars_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and code = (select code from t_calendar);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is(
    (select count(*) from ores_refdata_calendars_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and code = (select code from t_calendar)
       and valid_to = ores_utility_infinity_timestamp_fn()),
    1::bigint,
    'a tenant closes no shared calendar');

select is(
    (select count(*) from ores_iam_seed_profile_steps_tbl
     where step_kind = 'publish_bundle'
       and valid_to = ores_utility_infinity_timestamp_fn()
       and arguments_json -> 'bundles' ? 'world'),
    0::bigint,
    'no seed profile publishes the world catalogue into its tenant');

select * from finish();

rollback;
