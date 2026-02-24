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
-- Trade Query Functions
-- =============================================================================

-- Read the latest (active) trades for all books in a portfolio subtree.
--
-- Walks the portfolio hierarchy recursively starting from p_portfolio_id,
-- collects all books in that subtree, then returns the current versions of
-- all trades belonging to those books for the given tenant.
--
-- Column order matches the C++ raw_row_to_trade() mapper (21 columns):
--   0:id  1:tenant_id  2:version  3:party_id  4:external_id  5:book_id
--   6:portfolio_id  7:successor_trade_id  8:counterparty_id  9:trade_type
--   10:netting_set_id  11:lifecycle_event  12:trade_date  13:execution_timestamp
--   14:effective_date  15:termination_date  16:modified_by  17:performed_by
--   18:change_reason_code  19:change_commentary  20:valid_from
create or replace function ores_trading_read_trades_by_portfolio_fn(
    p_tenant_id   uuid,
    p_portfolio_id uuid,
    p_offset       bigint,
    p_limit        bigint
)
returns table (
    id                  uuid,
    tenant_id           uuid,
    version             integer,
    party_id            uuid,
    external_id         text,
    book_id             uuid,
    portfolio_id        uuid,
    successor_trade_id  uuid,
    counterparty_id     uuid,
    trade_type          text,
    netting_set_id      text,
    lifecycle_event     text,
    trade_date          date,
    execution_timestamp timestamp with time zone,
    effective_date      date,
    termination_date    date,
    modified_by         text,
    performed_by        text,
    change_reason_code  text,
    change_commentary   text,
    valid_from          timestamp with time zone
) as $$
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
    ),
    pb(id) as (
        select b.id
        from ores_refdata_books_tbl b
        where b.parent_portfolio_id in (select pt.id from pt)
          and b.valid_to = ores_utility_infinity_timestamp_fn()
    )
    select
        t.id,
        t.tenant_id,
        t.version,
        t.party_id,
        t.external_id,
        t.book_id,
        t.portfolio_id,
        t.successor_trade_id,
        t.counterparty_id,
        t.trade_type,
        t.netting_set_id,
        t.lifecycle_event,
        t.trade_date,
        t.execution_timestamp,
        t.effective_date,
        t.termination_date,
        t.modified_by,
        t.performed_by,
        t.change_reason_code,
        t.change_commentary,
        t.valid_from
    from ores_trading_trades_tbl t
    where t.tenant_id = p_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn()
      and t.book_id in (select pb.id from pb)
    order by t.id
    offset p_offset
    limit p_limit;
end;
$$ language plpgsql stable security definer;

comment on function ores_trading_read_trades_by_portfolio_fn(uuid, uuid, bigint, bigint) is
'Returns the latest active trades for all books in the given portfolio subtree,
 with pagination. Walks the full portfolio hierarchy recursively.';

-- Count the latest (active) trades for all books in a portfolio subtree.
--
-- Same recursive CTE logic as ores_trading_read_trades_by_portfolio_fn
-- but returns a single count for pagination purposes.
create or replace function ores_trading_count_trades_by_portfolio_fn(
    p_tenant_id    uuid,
    p_portfolio_id uuid
)
returns bigint as $$
declare
    v_count bigint;
begin
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
    ),
    pb(id) as (
        select b.id
        from ores_refdata_books_tbl b
        where b.parent_portfolio_id in (select pt.id from pt)
          and b.valid_to = ores_utility_infinity_timestamp_fn()
    )
    select count(*)
    into v_count
    from ores_trading_trades_tbl t
    where t.tenant_id = p_tenant_id
      and t.valid_to = ores_utility_infinity_timestamp_fn()
      and t.book_id in (select pb.id from pb);

    return coalesce(v_count, 0);
end;
$$ language plpgsql stable security definer;

comment on function ores_trading_count_trades_by_portfolio_fn(uuid, uuid) is
'Counts the latest active trades for all books in the given portfolio subtree.
 Counterpart to ores_trading_read_trades_by_portfolio_fn for pagination.';
