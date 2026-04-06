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

-- =============================================================================
-- Portfolio Book ID Helper
-- =============================================================================

-- Returns the IDs of all books belonging to any portfolio in the subtree
-- rooted at p_portfolio_id.
--
-- Walks the portfolio hierarchy recursively via parent_portfolio_id and
-- returns the UUID of every active book whose parent_portfolio_id falls within
-- that subtree. The C++ repository calls this to obtain the book_id set, then
-- queries ores_trading_trades_tbl directly using sqlgen for type-safe,
-- schema-coupled trade retrieval.
create or replace function ores_trading_get_book_ids_by_portfolio_fn(
    p_tenant_id    uuid,
    p_portfolio_id uuid
)
returns setof uuid as $$
begin
    return query
    with recursive pt(id) as (
        select p.id
        from ores_refdata_portfolios_tbl p
        where p.id = p_portfolio_id
          and p.valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select p.id
        from ores_refdata_portfolios_tbl p
        inner join pt on p.parent_portfolio_id = pt.id
        where p.valid_to = ores_utility_infinity_timestamp_fn()
    )
    select b.id
    from ores_refdata_books_tbl b
    where b.parent_portfolio_id in (select pt.id from pt)
      and b.valid_to = ores_utility_infinity_timestamp_fn();
end;
$$ language plpgsql stable security definer;

comment on function ores_trading_get_book_ids_by_portfolio_fn(uuid, uuid) is
'Returns the UUIDs of all books belonging to any portfolio in the subtree
 rooted at p_portfolio_id. Walks the portfolio hierarchy recursively via
 parent_portfolio_id.';

-- =============================================================================
-- Export Trade IDs by Book IDs
-- =============================================================================

/**
 * Returns the UUIDs of all active trades belonging to any of the supplied
 * book IDs.
 *
 * Used by the trading service export-to-storage handler to resolve the
 * trade set for a report without cross-schema dependencies.
 */
create or replace function ores_trading_get_trade_ids_by_books_fn(
    p_tenant_id  uuid,
    p_book_ids   uuid[]
)
returns setof uuid as $$
begin
    return query
        select t.id
        from ores_trading_trades_tbl t
        where t.tenant_id = p_tenant_id
          and t.book_id = any(p_book_ids)
          and t.valid_to = ores_utility_infinity_timestamp_fn()
        order by t.book_id, t.id;
end;
$$ language plpgsql stable security definer;

comment on function ores_trading_get_trade_ids_by_books_fn(uuid, uuid[]) is
'Returns the UUIDs of all active trades belonging to any of the supplied book
 IDs. Filtered by tenant_id and ordered by book_id, trade_id.';
