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
-- System Settings: party scope and the scoped read
-- =============================================================================
-- The table is generated from ores.variability.system_setting, but two things
-- it needs are this component's own domain logic and have no codegen
-- equivalent, so they live here rather than in the generated file:
--
--   1. A tenant-wide setting has no party to name, so the database resolves
--      the tenant's system party for it.
--   2. Components that read settings in process hold no direct SELECT grant
--      on the table, so they read through a SECURITY DEFINER function.

-- Resolves the party_id that scopes system-/tenant-wide settings for a
-- tenant: its refdata system party, or the nil UUID for the sentinel system
-- tenant (which has no refdata party -- it never goes through tenant
-- provisioning). Read time and write time both call it, so both resolve the
-- same scope for an omitted party_id.
create or replace function ores_variability_resolve_system_party_fn(
    p_tenant_id uuid
) returns uuid as $$
declare
    v_party_id uuid;
begin
    select id into v_party_id
    from ores_refdata_read_system_party_fn(p_tenant_id)
    limit 1;

    return coalesce(v_party_id, '00000000-0000-0000-0000-000000000000'::uuid);
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Defaults party_id to the tenant's system party when the writer states the
-- nil UUID, which is what a caller writing a tenant-wide setting sends: it has
-- no party to name. A party-specific setting states its own party_id and is
-- used as-is (soft FK -- no cross-schema constraint, consistent with the other
-- refdata cross-references).
--
-- This is a trigger of its own rather than part of the generated insert
-- trigger, because the generated trigger is codegen's and this rule is not.
-- Postgres fires same-timing triggers in name order, so this one runs after
-- the generated one; that is harmless, because the generated trigger manages
-- the version by the row's id and never reads party_id. Every BEFORE trigger
-- still runs before the row lands, so the composite unique index sees the
-- resolved party.
create or replace function ores_variability_system_settings_default_party_fn()
returns trigger as $$
begin
    if new.party_id is null
       or new.party_id = '00000000-0000-0000-0000-000000000000'::uuid then
        new.party_id := ores_variability_resolve_system_party_fn(new.tenant_id);
    end if;

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_variability_system_settings_party_default_trg
before insert on "ores_variability_system_settings_tbl"
for each row
execute function ores_variability_system_settings_default_party_fn();

-- SECURITY DEFINER: called from service contexts (e.g. IAM) that hold no direct
-- SELECT grant on ores_variability_system_settings_tbl. The function owner (DDL
-- user) performs the read; the caller needs only EXECUTE on this function.
--
-- Reads the settings visible to one scope: the tenant's system party for
-- system/tenant-level flags, or a specific party for party-level flags such as
-- onboarding.party. It returns the row's id and party beside its value, not the
-- value alone, because a caller that writes a setting back has to name the row
-- it is replacing, and the party the database resolved for it is not a value
-- the caller ever stated.
create or replace function ores_variability_get_system_settings_fn(
    p_tenant_id uuid,
    p_party_id uuid default null
) returns table(setting_id uuid,
                setting_name text,
                setting_party_id uuid,
                setting_value text,
                setting_data_type text,
                setting_description text) as $$
    select id, name, party_id, value, data_type, description
    from ores_variability_system_settings_tbl
    where tenant_id = p_tenant_id
      and party_id = coalesce(p_party_id, ores_variability_resolve_system_party_fn(p_tenant_id))
      and valid_to = ores_utility_infinity_timestamp_fn()
    order by name;
$$ language sql stable security definer set search_path = public, pg_temp;

-- -----------------------------------------------------------------------------
-- Least privilege
-- -----------------------------------------------------------------------------
-- Both functions above are SECURITY DEFINER, so they run as their owner and
-- bypass row-level security by design: that is what lets a service context read
-- settings at all, and what lets a tenant-wide write resolve a party. A definer
-- function that takes the tenant as a parameter is therefore only safe if the
-- set of roles that may call it is the set of roles that are meant to see
-- across tenants. PostgreSQL grants EXECUTE to PUBLIC on every new function, so
-- without the revoke below every role in the database -- including each of the
-- other services -- could call it with any tenant's id and read that tenant's
-- settings.
--
-- The revokes are not decoration. A new function is world-executable until
-- told otherwise, and nothing else in this schema says otherwise.

revoke execute on function ores_variability_resolve_system_party_fn(uuid)
    from public;

revoke execute on function ores_variability_get_system_settings_fn(uuid, uuid)
    from public;

-- The three roles that read settings through the definer function rather than
-- holding a direct grant on the table: variability's own service and the two
-- that read settings in process, iam and http.
grant execute on function ores_variability_get_system_settings_fn(uuid, uuid)
    to :"variability_service_user", :"iam_service_user", :"http_user";
