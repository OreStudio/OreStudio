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
-- Business Unit Book ID Helper
-- =============================================================================

-- Returns the IDs of all books owned by any business unit in the subtree
-- rooted at p_business_unit_id.
--
-- Walks the BU hierarchy recursively via parent_business_unit_id and returns
-- the UUID of every active book whose owner_unit_id falls within that subtree.
-- The C++ repository calls this to obtain the book_id set, then queries
-- ores_trading_trades_tbl directly using sqlgen for type-safe, schema-coupled
-- trade retrieval.
create or replace function ores_trading_get_book_ids_by_business_unit_fn(
    p_tenant_id        uuid,
    p_business_unit_id uuid
)
returns setof uuid as $$
begin
    return query
    with recursive bu(id) as (
        select u.id
        from ores_refdata_business_units_tbl u
        where u.id = p_business_unit_id
          and u.valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select u.id
        from ores_refdata_business_units_tbl u
        inner join bu on u.parent_business_unit_id = bu.id
        where u.valid_to = ores_utility_infinity_timestamp_fn()
    )
    select b.id
    from ores_refdata_books_tbl b
    where b.owner_unit_id in (select bu.id from bu)
      and b.valid_to = ores_utility_infinity_timestamp_fn();
end;
$$ language plpgsql stable security definer;

comment on function ores_trading_get_book_ids_by_business_unit_fn(uuid, uuid) is
'Returns the UUIDs of all books owned by any business unit in the subtree
 rooted at p_business_unit_id. Walks the BU hierarchy recursively via
 parent_business_unit_id and collects books via owner_unit_id.';
