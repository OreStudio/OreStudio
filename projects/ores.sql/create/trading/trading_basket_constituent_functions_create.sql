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
-- Whether a basket constituent's code exists in the catalogue its kind names
-- =============================================================================

-- A constituent states two things: a kind and a code. Neither is meaningful
-- alone. 'RIC:.SPX' is an underlying only because its kind says equity, and an
-- equity only because the equity catalogue holds it, so the refusal belongs
-- where both are in hand -- in the row that states them -- and not in a report
-- that reads them back later.
--
-- The check on ores_trading_equity_basket_constituents_tbl calls this with the
-- row's two columns. It raises rather than returning false so the operator
-- reads the kind and the code that disagree instead of a constraint name.
--
-- The catalogues are tenant scoped and carry their own policies, so this runs
-- as the writing account and sees the catalogues that account may see: a
-- tenant cannot name an underlying another tenant published.
create or replace function ores_trading_basket_constituent_resolves_fn(
    p_kind text,
    p_code text
) returns boolean as $$
declare
    v_found boolean := false;
    v_kind text := lower(p_kind);
begin
    if v_kind = 'equity' then
        select exists (
            select 1 from ores_refdata_equity_underlyings_tbl u
            where u.code = p_code
              and u.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_found;
    elsif v_kind = 'fx' then
        select exists (
            select 1 from ores_refdata_currency_pairs_tbl p
            where p.code = p_code
              and p.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_found;
    elsif v_kind = 'commodity' then
        select exists (
            select 1 from ores_refdata_commodities_tbl c
            where c.code = p_code
              and c.valid_to = ores_utility_infinity_timestamp_fn()
        ) into v_found;
    else
        raise exception
            'A basket constituent states kind ''%'', and the store holds no such vocabulary. Known kinds: equity, fx, commodity.',
            p_kind
            using errcode = 'check_violation';
    end if;

    if not v_found then
        raise exception
            'A basket constituent of kind ''%'' names ''%'', and the % catalogue does not hold it.',
            p_kind, p_code, v_kind
            using errcode = 'check_violation';
    end if;

    return true;
end;
$$ language plpgsql;
