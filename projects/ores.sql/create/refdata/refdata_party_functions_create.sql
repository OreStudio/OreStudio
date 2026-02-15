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
-- Party Query Functions
-- =============================================================================

-- Read the system party for a given tenant.
-- Every tenant has exactly one system party (party_category='System') which
-- serves as the root of the party hierarchy. Returns the current (active)
-- version of the system party.
create or replace function ores_refdata_read_system_party_fn(
    p_tenant_id uuid
)
returns table (
    id uuid,
    tenant_id uuid,
    version integer,
    full_name text,
    short_code text,
    party_category text,
    party_type text,
    parent_party_id uuid,
    business_center_code text,
    status text,
    modified_by text,
    performed_by text,
    change_reason_code text,
    change_commentary text,
    valid_from timestamp with time zone,
    valid_to timestamp with time zone
) as $$
begin
    return query
    select
        p.id,
        p.tenant_id,
        p.version,
        p.full_name,
        p.short_code,
        p.party_category,
        p.party_type,
        p.parent_party_id,
        p.business_center_code,
        p.status,
        p.modified_by,
        p.performed_by,
        p.change_reason_code,
        p.change_commentary,
        p.valid_from,
        p.valid_to
    from ores_refdata_parties_tbl p
    where p.tenant_id = p_tenant_id
      and p.party_category = 'System'
      and p.valid_to = ores_utility_infinity_timestamp_fn();
end;
$$ language plpgsql security definer;

-- Compute the visible party set for a given party within a tenant.
-- Returns an array of UUIDs containing the given party and all its descendants
-- in the party hierarchy. Used to populate app.visible_party_ids session
-- variable for party-level RLS.
create or replace function ores_refdata_visible_party_ids_fn(
    p_tenant_id uuid,
    p_party_id uuid
) returns uuid[] as $$
begin
    return (
        WITH RECURSIVE party_tree AS (
            SELECT id FROM ores_refdata_parties_tbl
            WHERE id = p_party_id
              AND tenant_id = p_tenant_id
              AND valid_to = ores_utility_infinity_timestamp_fn()
            UNION ALL
            SELECT p.id FROM ores_refdata_parties_tbl p
            JOIN party_tree pt ON p.parent_party_id = pt.id
            WHERE p.tenant_id = p_tenant_id
              AND p.valid_to = ores_utility_infinity_timestamp_fn()
        )
        SELECT array_agg(id) FROM party_tree
    );
end;
$$ language plpgsql stable security definer;
