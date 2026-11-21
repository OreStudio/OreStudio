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
 * pgTAP tests for the tenant scope of the basket constituent catalogue check.
 *
 * A basket constituent names a kind and a code, and the code has to exist in
 * the catalogue its kind names. The check runs as a security definer function,
 * so no policy binds it and it states the shared-reference rule itself: the
 * code names a row of the caller's tenant or a shared row of the system
 * tenant, and never another tenant's row.
 *
 * The facts asserted are:
 *
 * - the system tenant resolves a world commodity;
 * - the system tenant refuses a code only a tenant holds;
 * - a tenant resolves a code it holds itself;
 * - a tenant resolves a world commodity, which it reads where it lies;
 * - a tenant refuses a code no catalogue holds.
 *
 * Run as ORES_TEST_DB_USER, never as the owner role, which bypasses row-level
 * security: pg_prove -d <database> test/trading_basket_constituent_tenant_scope_test.sql
 */

begin;

select plan(5);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_world_code on commit drop as
select code from ores_refdata_commodities_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and valid_to = ores_utility_infinity_timestamp_fn()
order by code
limit 1;

select is(
    ores_trading_basket_constituent_resolves_fn('commodity', (select code from t_world_code)),
    true,
    'the system tenant resolves a world commodity');

create temporary table t_tenant on commit drop as
select tenant_id as id from ores_iam_provision_tenant_fn(
    'production', 'test_catalogue_scope', 'Catalogue Scope Fixture', 'catalogue_scope');

-- A commodity only this tenant will hold. It is staged in the world dataset so
-- the publish function writes it, which is the only path that writes a
-- catalogue row.
insert into ores_dq_commodities_artefact_tbl (
    dataset_id, tenant_id, code, version, name, description, unit_code,
    quotation_currency_code)
select id, ores_utility_system_tenant_id_fn(), 'PGTAP.PRIVATE', 0,
    'pgTAP private commodity', 'A commodity one tenant holds and no other.',
    'PTG', 'USD'
from ores_dq_datasets_tbl
where tenant_id = ores_utility_system_tenant_id_fn()
  and code = 'refdata.commodities'
  and valid_to = ores_utility_infinity_timestamp_fn();

select * from ores_refdata_publish_commodities_from_dq_fn(
    (select id from ores_dq_datasets_tbl
      where tenant_id = ores_utility_system_tenant_id_fn()
        and code = 'refdata.commodities'
        and valid_to = ores_utility_infinity_timestamp_fn()),
    (select id from t_tenant), 'upsert', '{}'::jsonb);

select throws_ok(
    $$select ores_trading_basket_constituent_resolves_fn('commodity', 'PGTAP.PRIVATE')$$,
    '23514',
    null,
    'the system tenant refuses a code only a tenant holds');

select set_config('app.current_tenant_id', (select id::text from t_tenant), true);

select is(
    ores_trading_basket_constituent_resolves_fn('commodity', 'PGTAP.PRIVATE'),
    true,
    'a tenant resolves a code it holds itself');

select is(
    ores_trading_basket_constituent_resolves_fn('commodity', (select code from t_world_code)),
    true,
    'a tenant resolves a world commodity it reads where it lies');

select throws_ok(
    $$select ores_trading_basket_constituent_resolves_fn('commodity', 'NO.SUCH.COMMODITY')$$,
    '23514',
    null,
    'a tenant refuses a code no catalogue holds');

select * from finish();

rollback;
