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
-- Session Context Functions
-- =============================================================================
--
-- The readers of the session settings the application layer sets on every
-- connection: tenant, party, visible parties, actor and service. They read
-- nothing but current_setting, so create.sql creates them before any
-- component's tables, and row level security policies anywhere in the schema
-- can call them.

-- Get current tenant ID from session variable
create or replace function ores_iam_current_tenant_id_fn()
returns uuid as $$
begin
    return current_setting('app.current_tenant_id', true)::uuid;
exception
    when others then
        return null;
end;
$$ language plpgsql stable;

-- Get current party ID from session variable
create or replace function ores_iam_current_party_id_fn()
returns uuid as $$
begin
    return current_setting('app.current_party_id', true)::uuid;
exception
    when others then
        return null;
end;
$$ language plpgsql stable;

-- Get current actor username from session variable.
-- Set by the application layer before calling privileged SECURITY DEFINER functions.
create or replace function ores_iam_current_actor_fn()
returns text as $$
begin
    return nullif(current_setting('app.current_actor', true), '');
exception
    when others then
        return null;
end;
$$ language plpgsql stable;

-- Get current service account from session variable.
-- Set by the application layer to identify the service performing the write.
-- Used by insert triggers to stamp performed_by with the service identity.
create or replace function ores_iam_current_service_fn()
returns text as $$
begin
    return nullif(current_setting('app.current_service', true), '');
exception
    when others then
        return null;
end;
$$ language plpgsql stable;

-- Get visible party IDs from session variable as uuid array
create or replace function ores_iam_visible_party_ids_fn()
returns uuid[] as $$
begin
    return current_setting('app.visible_party_ids', true)::uuid[];
exception
    when others then
        return null;
end;
$$ language plpgsql stable;
