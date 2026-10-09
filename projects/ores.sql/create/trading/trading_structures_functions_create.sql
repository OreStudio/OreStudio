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
-- A structure's internal version, read from its legs
-- =============================================================================

-- The version is not stored beside the deal: it is folded from what the deal
-- is made of, so no counter can drift from it. A leg's own version moves when
-- any of its components is written, because those children bump it, and the
-- membership carries its own version, so a leg linked or unlinked moves the
-- deal's version too.
--
-- The external version the customer confirms lives on the agreement, which the
-- authorisation and agreement work owns; this is the internal one.

create or replace function ores_trading_structure_version_fn(
    p_tenant_id    uuid,
    p_structure_id uuid
)
returns integer as $$
declare
    v_legs integer;
    v_own  integer;
begin
    select max(greatest(m.version, t.version))
    into v_legs
    from ores_trading_structure_members_tbl m
    join ores_trading_trades_tbl t
      on t.tenant_id = m.tenant_id
     and t.id = m.trade_id
     and t.valid_to = ores_utility_infinity_timestamp_fn()
    where m.tenant_id = p_tenant_id
      and m.structure_id = p_structure_id;

    select version
    into v_own
    from ores_trading_structures_tbl
    where tenant_id = p_tenant_id
      and id = p_structure_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    return greatest(coalesce(v_own, 0), coalesce(v_legs, 0));
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;
