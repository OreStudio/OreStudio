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
 * Each overnight-index curve config of the system tenant is complete: it names
 * an overnight index convention, states that convention's day count in the
 * engine's code, ladders the seven pillars every curve shares, reads one ladder
 * series of the synthetic Bloomberg feed, and writes its own output series.
 *
 * Run as ORES_TEST_DB_USER: pg_prove -d <database> test/refdata_overnight_curve_configs_test.sql
 */

begin;

select plan(7);

select set_config('app.current_tenant_id', ores_utility_system_tenant_id_fn()::text, true);

create temporary table t_configs on commit drop as
select c.id, c.name, c.currency_code, c.day_count_convention, c.output_series_id
from ores_refdata_ir_curve_bootstrap_configs_tbl c
where c.tenant_id = ores_utility_system_tenant_id_fn()
  and c.change_reason_code = 'system.initial_load'
  and c.valid_to = ores_utility_infinity_timestamp_fn();

select is(
    (select count(*)::int from t_configs where name <> 'USD-SOFR-FOMC'),
    19,
    'the seed holds an overnight-index curve config for nineteen currencies besides USD'
);

select is(
    (select count(*)::int from t_configs
     where currency_code in ('AUD', 'CAD', 'CHF', 'EUR', 'GBP', 'JPY', 'NZD', 'USD')),
    8,
    'every currency of the derived FX pairs has a curve config'
);

select is(
    (select count(*)::int from t_configs t
     where t.name <> 'USD-SOFR-FOMC'
       and not exists (
           select 1 from ores_refdata_overnight_index_conventions_tbl o
           where o.id = t.name
             and o.tenant_id = ores_utility_system_tenant_id_fn()
             and o.valid_to = ores_utility_infinity_timestamp_fn()
             and ((t.day_count_convention = 'A360' and o.day_count_fraction = 'ACT/360')
               or (t.day_count_convention = 'A365F' and o.day_count_fraction = 'ACT/365.FIXED')))),
    0,
    'every config is named after an overnight index convention and states its day count'
);

select is(
    (select count(*)::int from t_configs t
     where (select string_agg(p.end_tenor_code || ':' || p.curve_role_code, ',' order by p.sequence_index)
            from ores_refdata_ir_curve_bootstrap_pillars_tbl p
            where p.bootstrap_config_id = t.id
              and p.valid_to = ores_utility_infinity_timestamp_fn())
           is distinct from '3M:DEPOSIT,6M:FRA,12M:SWAP,2Y:SWAP,5Y:SWAP,10Y:SWAP,30Y:SWAP'),
    0,
    'every config ladders a 3M deposit, a 6M FRA and swaps to 12M, 2Y, 5Y, 10Y and 30Y'
);

select is(
    (select count(*)::int from t_configs t
     where (select count(distinct p.market_series_id)
            from ores_refdata_ir_curve_bootstrap_pillars_tbl p
            where p.bootstrap_config_id = t.id
              and p.valid_to = ores_utility_infinity_timestamp_fn()) <> 1),
    0,
    'every config reads one ladder series'
);

select is(
    (select count(*)::int from t_configs t
     join ores_refdata_ir_curve_bootstrap_pillars_tbl p
       on p.bootstrap_config_id = t.id
      and p.sequence_index = 0
      and p.valid_to = ores_utility_infinity_timestamp_fn()
     where not exists (
           select 1 from ores_marketdata_market_series_tbl s
           where s.id = p.market_series_id
             and s.provider_code = 'bloomberg'
             and s.oresmd_uri = 'oresmd://ir/' || t.currency_code
                 || '?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D'
             and s.valid_to = ores_utility_infinity_timestamp_fn())),
    0,
    'every ladder is the currency''s spot-starting overnight-index swap series of the synthetic Bloomberg feed'
);

select is(
    (select count(*)::int from t_configs t
     where not exists (
           select 1 from ores_marketdata_market_series_tbl s
           where s.id = t.output_series_id
             and s.oresmd_uri = 'oresmd://ir/' || t.currency_code
                 || '?type=series&instrument=discount&quote=rate&curve_id=' || t.name
             and s.valid_to = ores_utility_infinity_timestamp_fn())),
    0,
    'every config writes its own discount series, named by the config'
);

select * from finish();

rollback;
