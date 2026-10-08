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
-- Party visibility for a row that belongs to a trade or a structure
-- =============================================================================

-- A child of a trade or a structure holds no party of its own: the party is a
-- fact about the deal, and the child reaches it through the key it already
-- carries. These two functions are the reach. They are security definer
-- because a policy that subqueries the anchor is itself subject to the
-- anchor's policies, and inlining that join in forty policies would put the
-- recursion in forty places instead of one.
--
-- Both return null when the anchor has no current row, which makes the policy
-- that calls them deny rather than pass.

create or replace function ores_trading_trade_party_fn(
    p_tenant_id uuid,
    p_trade_id  uuid
)
returns uuid as $$
begin
    return (
        select party_id
        from ores_trading_trades_tbl
        where tenant_id = p_tenant_id
          and id = p_trade_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

create or replace function ores_trading_structure_party_fn(
    p_tenant_id    uuid,
    p_structure_id uuid
)
returns uuid as $$
begin
    return (
        select party_id
        from ores_trading_structures_tbl
        where tenant_id = p_tenant_id
          and id = p_structure_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;
