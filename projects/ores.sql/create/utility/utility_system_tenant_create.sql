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

-- System tenant UUID constant (max UUID per RFC 9562 sentinel value).
-- Loaded before all component schemas so it is available in CHECK constraints
-- at DDL time regardless of schema loading order.
-- Using max UUID instead of nil UUID prevents confusion with uninitialized UUIDs.
create or replace function ores_utility_system_tenant_id_fn()
returns uuid as $$
begin
    return 'ffffffff-ffff-ffff-ffff-ffffffffffff'::uuid;
end;
$$ language plpgsql immutable;

-- Nil UUID constant (RFC 9562 nil UUID: all zeroes).
-- Used in CHECK constraints to reject uninitialized or zero-value UUIDs.
create or replace function ores_utility_nil_uuid_fn()
returns uuid as $$
begin
    return '00000000-0000-0000-0000-000000000000'::uuid;
end;
$$ language plpgsql immutable;

-- Live workspace sentinel UUID (version nibble 'a' is outside the valid UUID
-- version range 1-8, so no generator will ever produce this value).
-- Used as the stable well-known ID of the Live (root) workspace.
-- The nil UUID is intentionally not used: it is the C++ default for
-- boost::uuids::uuid, so an uninitialised workspace_id would silently resolve
-- to Live. The max UUID is reserved for the system tenant.
-- C++ equivalent: ores::utility::uuid::live_workspace_uuid_str
create or replace function ores_utility_live_workspace_id_fn()
returns uuid as $$
begin
    return 'aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa'::uuid;
end;
$$ language plpgsql immutable;
