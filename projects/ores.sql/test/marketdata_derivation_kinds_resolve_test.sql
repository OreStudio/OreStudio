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
 * pgTAP tests for what a series' derivation states.
 *
 * A series says how its value was made, and the kind decides the table its
 * config id resolves against. Two rules follow, and both are asserted here:
 *
 * - one kind names one table, so an =IR_CURVE_BOOTSTRAP= config id is a refdata
 *   blueprint and an =IR_TERM_FIXING= one is a synthetic generation config;
 * - a seeded curve states its derivation before anything republishes it, because
 *   nothing else stamps it and a curve that reads observed would tell a screen
 *   the opposite of the truth.
 *
 * Run with: pg_prove -d <database> test/marketdata_derivation_kinds_resolve_test.sql
 */

begin;

select plan(3);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

select is(
    (select count(*)
     from ores_marketdata_market_series_tbl s
     where s.valid_to = ores_utility_infinity_timestamp_fn()
       and s.derivation_kind = 'IR_CURVE_BOOTSTRAP'
       and not exists (
           select 1
           from ores_refdata_ir_curve_bootstrap_configs_tbl c
           where c.id = s.derivation_config_id
             and c.tenant_id = s.tenant_id
             and c.valid_to = ores_utility_infinity_timestamp_fn())),
    0::bigint,
    'every IR_CURVE_BOOTSTRAP config id resolves to a blueprint');

select is(
    (select count(*)
     from ores_marketdata_market_series_tbl s
     where s.valid_to = ores_utility_infinity_timestamp_fn()
       and s.derivation_kind = 'IR_TERM_FIXING'
       and not exists (
           select 1
           from ores_synthetic_ir_curve_generation_configs_tbl c
           where c.id = s.derivation_config_id
             and c.tenant_id = s.tenant_id
             and c.valid_to = ores_utility_infinity_timestamp_fn())),
    0::bigint,
    'every IR_TERM_FIXING config id resolves to a generation config');

select is(
    (select count(*)
     from ores_refdata_ir_curve_bootstrap_configs_tbl c
     left join ores_marketdata_market_series_tbl s
       on s.id = c.output_series_id
      and s.tenant_id = c.tenant_id
      and s.valid_to = ores_utility_infinity_timestamp_fn()
     where c.valid_to = ores_utility_infinity_timestamp_fn()
       and (s.id is null or s.derivation_kind <> 'IR_CURVE_BOOTSTRAP')),
    0::bigint,
    'a seeded curve states its derivation before any republish, and has a series');

select * from finish();

rollback;
