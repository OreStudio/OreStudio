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
 * pgTAP tests for the two lookups a sensitivity template names.
 *
 * The methods are the five ORE defines, and the one that prices no class of
 * product says so. The asset classes are a bridge: the reference data's code, the
 * prefix ORE spells in a template key, and the words a person reads. The prefix is
 * not the code -- ORE spells commodity COMM in a template key where the rest of
 * ORE spells it COM -- so the two are joined here and not in code.
 *
 * The catalogue view composes the two names into the label a person chooses a
 * template by, and a trade-level template's label is the method alone.
 *
 * Tests cover:
 * - The seed holds five methods and five asset classes
 * - One method, Trade, needs no asset class
 * - Every bridge row names an asset class the reference data holds
 * - The prefixes are the ones ORE presents, and commodity's is COMM
 * - A bridge row naming a class the reference data does not hold is refused
 * - The catalogue labels a classed key with both names
 * - The catalogue labels a trade-level key with the method alone
 *
 * Run with: pg_prove -d <database> test/analytics_sensitivity_lookups_test.sql
 */

begin;

select plan(8);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);
select set_config('app.current_actor', 'super_admin', true);

-- =============================================================================
-- The seed
-- =============================================================================

select is(
    (select count(*) from ores_analytics_sensitivity_methods_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    5::bigint,
    'the seed holds the five methods ORE defines');

select is(
    (select count(*) from ores_analytics_sensitivity_asset_classes_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    5::bigint,
    'the seed holds the five asset classes ORE presents a template prefix for');

select is(
    (select string_agg(code::text, ', ' order by display_order)
     from ores_analytics_sensitivity_methods_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()
       and not needs_asset_class),
    'Trade',
    'the trade-level method is the only one that needs no asset class');

select is(
    (select count(*) from ores_analytics_sensitivity_asset_classes_tbl a
     where a.tenant_id = ores_utility_system_tenant_id_fn()
       and a.valid_to = ores_utility_infinity_timestamp_fn()
       and not exists (
           select 1 from ores_refdata_asset_class_codes_tbl r
           where r.tenant_id = ores_utility_system_tenant_id_fn()
             and r.code = a.asset_class_code
             and r.valid_to = ores_utility_infinity_timestamp_fn())),
    0::bigint,
    'every bridge row names an asset class the reference data holds');

select is(
    (select string_agg(template_prefix::text, ', ' order by display_order)
     from ores_analytics_sensitivity_asset_classes_tbl
     where tenant_id = ores_utility_system_tenant_id_fn()
       and valid_to = ores_utility_infinity_timestamp_fn()),
    'IR, FX, EQ, COMM, CR',
    'the prefixes are the ones ORE spells in its template keys');

-- =============================================================================
-- The bridge validates what it names
-- =============================================================================

select throws_ok(
    $$insert into ores_analytics_sensitivity_asset_classes_tbl (
          asset_class_code, tenant_id, version, template_prefix, name, description,
          display_order, modified_by, performed_by, change_reason_code, change_commentary
      ) values (
          'weather', ores_utility_system_tenant_id_fn(), 0, 'WX', 'Weather',
          'Products whose value follows the weather.', 6, 'super_admin', 'super_admin',
          'system.test', 'Refuse a class the reference data does not hold')$$,
    '23503', null, 'the bridge refuses an asset class the reference data does not hold');

-- =============================================================================
-- The catalogue composes the two names
-- =============================================================================

select is(
    (select label from ores_analytics_sensitivity_template_catalogue_v
     where value = 'IR_Analytical'),
    'Interest rate · Analytical',
    'a classed key is labelled with the class and the method');

select is(
    (select label from ores_analytics_sensitivity_template_catalogue_v
     where value = 'SensiTrade'),
    'Trade level',
    'a trade-level key is labelled with the method alone');

select * from finish();

rollback;
