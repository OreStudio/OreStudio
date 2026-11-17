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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

-- =============================================================================
-- The market data a set of trades names, as oresmd addresses
-- =============================================================================

-- A trade names its market data in the columns of its instruments: an index on
-- a leg, an underlying, a credit curve. Each of those columns holds an oresmd
-- address, and an address is the identity of the series that answers it, so
-- the requirements of a set of trades are the addresses its rows hold. A
-- column that still holds a bare ORE name is not a requirement this function
-- can state; the caller sees that trade with no row.
--
-- The rows are read as they stood at p_as_of, so two runs over the same trades
-- on different dates read the instruments each date held. A null p_as_of reads
-- the current rows, which a write earlier in the same transaction is one of.
--
-- The function does not filter by party: row-level security limits the rows
-- to the parties the session may see.
--
-- A column joins the list below when a trade table starts to hold an address
-- in it. The pair is the table and the column; every table in it is keyed by
-- trade_id. Bond issues are keyed by issue and reached through the trade's
-- bond instrument, so they are read separately.
create or replace function ores_trading_market_references_fn(
    p_tenant_id uuid,
    p_trade_ids uuid[],
    p_as_of     timestamptz default null
)
returns table (trade_id uuid, oresmd_uri text, source text) as $$
declare
    v_columns constant text[][] := array[
        ['ores_trading_swap_legs_tbl', 'floating_index_code'],
        ['ores_trading_swap_legs_tbl', 'equity_name'],
        ['ores_trading_fra_instruments_tbl', 'rate_index'],
        ['ores_trading_bond_leg_rates_tbl', 'index'],
        ['ores_trading_bond_leg_rates_tbl', 'front_stub_short_index'],
        ['ores_trading_bond_leg_rates_tbl', 'front_stub_long_index'],
        ['ores_trading_bond_leg_rates_tbl', 'back_stub_short_index'],
        ['ores_trading_bond_leg_rates_tbl', 'back_stub_long_index'],
        ['ores_trading_inflation_swap_instruments_tbl', 'inflation_index_code'],
        ['ores_trading_instrument_underlyings_tbl', 'name'],
        ['ores_trading_scripted_parameter_values_tbl', 'value'],
        ['ores_trading_commodity_instruments_tbl', 'commodity_code'],
        ['ores_trading_commodity_instruments_tbl', 'spread_commodity_code'],
        ['ores_trading_commodity_basket_constituents_tbl', 'underlying_code'],
        ['ores_trading_equity_option_instruments_tbl', 'underlying_name'],
        ['ores_trading_equity_barrier_option_instruments_tbl', 'underlying_name'],
        ['ores_trading_equity_digital_option_instruments_tbl', 'underlying_name'],
        ['ores_trading_equity_digital_option_instruments_tbl', 'barrier_fixing_index'],
        ['ores_trading_equity_position_instruments_tbl', 'underlying_name'],
        ['ores_trading_equity_accumulator_instruments_tbl', 'underlying_name'],
        ['ores_trading_equity_variance_swap_instruments_tbl', 'underlying_name'],
        ['ores_trading_equity_asian_option_instruments_tbl', 'underlying_name'],
        ['ores_trading_equity_forward_instruments_tbl', 'underlying_name'],
        ['ores_trading_equity_swap_instruments_tbl', 'underlying_name'],
        ['ores_trading_equity_swap_instruments_tbl', 'floating_index'],
        ['ores_trading_credit_instruments_tbl', 'reference_entity_id'],
        ['ores_trading_cds_instruments_tbl', 'credit_curve_id'],
        ['ores_trading_index_cds_instruments_tbl', 'credit_curve_id'],
        ['ores_trading_credit_linked_swap_instruments_tbl', 'credit_curve_id'],
        ['ores_trading_risk_participation_instruments_tbl', 'credit_curve_id'],
        ['ores_trading_synthetic_cdo_instruments_tbl', 'qualifier']
    ];
    v_i integer;
begin
    for v_i in 1 .. array_length(v_columns, 1) loop
        return query execute format(
            'select distinct r.trade_id, r.%2$I::text, %3$L::text
             from %1$I r
             where r.tenant_id = $1
               and r.trade_id = any($2)
               and (($3 is null and r.valid_to = ores_utility_infinity_timestamp_fn())
                    or (r.valid_from <= $3 and r.valid_to > $3))
               and r.%2$I::text like ''oresmd://%%''',
            v_columns[v_i][1], v_columns[v_i][2],
            v_columns[v_i][1] || '.' || v_columns[v_i][2])
        using p_tenant_id, p_trade_ids, p_as_of;
    end loop;

    return query
    select distinct b.trade_id, u.uri, 'ores_trading_bond_issues_tbl.' || u.col
    from ores_trading_bond_instruments_tbl b
    join ores_trading_bond_issues_tbl i
      on i.tenant_id = b.tenant_id and i.issue_id = b.issue_id
     and ((p_as_of is null and i.valid_to = ores_utility_infinity_timestamp_fn())
          or (i.valid_from <= p_as_of and i.valid_to > p_as_of))
    cross join lateral (values
        ('credit_curve_id', i.credit_curve_id),
        ('reference_curve_id', i.reference_curve_id),
        ('income_curve_id', i.income_curve_id),
        ('volatility_curve_id', i.volatility_curve_id)) as u(col, uri)
    where b.tenant_id = p_tenant_id
      and b.trade_id = any(p_trade_ids)
      and ((p_as_of is null and b.valid_to = ores_utility_infinity_timestamp_fn())
           or (b.valid_from <= p_as_of and b.valid_to > p_as_of))
      and u.uri like 'oresmd://%';
end;
$$ language plpgsql stable;
