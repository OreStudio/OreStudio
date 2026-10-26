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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: sql_schema_domain_entity_create.mustache
 * To modify, update the template and regenerate.
 *
 * Engine Settings Table
 *
 * The central row of a named engine settings configuration. A report definition binds the configuration through
 * report_configuration, in the slot of configuration type engine_settings, as it binds its pricing engines. The row holds
 * only the identity. The settings live in three parts, each its own table keyed by this row: engine_settings_fixings,
 * engine_settings_building and engine_settings_execution. A part that has no row means the engine default for every
 * setting in it. A write to any part bumps the version of this row in the same transaction.
 */

create table if not exists "ores_reporting_engine_settings_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "name" text not null,
    "description" text null,
    "configuration_id" uuid not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> ores_utility_nil_uuid_fn())
);

-- Composite natural key: unique combination for active records
create unique index if not exists engine_settings_party_id_name_uniq_idx
on "ores_reporting_engine_settings_tbl" (tenant_id, party_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists engine_settings_version_uniq_idx
on "ores_reporting_engine_settings_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists engine_settings_id_uniq_idx
on "ores_reporting_engine_settings_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists engine_settings_tenant_idx
on "ores_reporting_engine_settings_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();


-- =============================================================================
-- Touch-version function for engine_settings
-- Bumps this entity's version without changing any of its own columns,
-- called by a child entity's insert trigger / delete rule when that
-- child is flagged :bump_parent_version: true against this entity. See
-- the "Temporal composite entity versioning" architecture doc.
--
-- Deliberately does NOT duplicate the version/valid_from/valid_to
-- management here: it fetches the current row, states the version it
-- read, and re-inserts — the table's own insert trigger (below) then
-- performs the exact same close-current/bump-version dance a
-- normal application save does. Stating the version read, rather than
-- the zero sentinel, is what makes the store see a replace: zero means
-- "no current row exists", so a touch that wrote zero over the live row
-- it had just read would be refused as a create that collides. Because
-- the read above holds the row lock, the version it states is still
-- current when the trigger re-reads it, so the compare-and-swap passes
-- for the same reason an application's own stated version does. This
-- also sidesteps a real footgun:
-- current_timestamp is frozen for the whole transaction, so a
-- same-transaction bulk import (parent + child created together, as
-- GLEIF provisioning does) would otherwise make the just-inserted
-- parent row's own valid_from collide with this function's close
-- timestamp.
--
-- Defined before the insert trigger/delete rule below (not after):
-- for a self-referencing entity (this entity is its own composite
-- parent, e.g. a portfolio tree), the delete rule calling this
-- function lives in the *same* file. PostgreSQL parses CREATE RULE
-- eagerly, so the function must already exist by then -- unlike a
-- PL/pgSQL function body (e.g. the insert trigger function), which is
-- opaque at CREATE time and only resolves calls at execution.
--
-- p_reason_code is passed through as-is (already a validated code from
-- the child's own row); p_child_entity distinguishes which child
-- triggered the bump in the free-text commentary.
-- =============================================================================
create or replace function ores_reporting_engine_settings_touch_version_fn(
    p_tenant_id uuid,
    p_id uuid,
    p_reason_code text,
    p_commentary text,
    p_modified_by text,
    p_performed_by text,
    p_child_entity text
) returns void as $$
declare
    rec ores_reporting_engine_settings_tbl%rowtype;
begin
    -- for update: takes the same row lock the parent's own insert
    -- trigger takes, so the snapshot in rec can't be based on a
    -- business-column value a concurrent direct edit is about to
    -- change — without this, that concurrent edit could be silently
    -- reverted once this function's later insert proceeds. See the
    -- "Temporal composite entity versioning" architecture doc,
    -- Concurrency section.
    select * into rec
    from "ores_reporting_engine_settings_tbl"
    where tenant_id = p_tenant_id
      and id = p_id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if not found then
        return;
    end if;

    -- Left at the version just read: the insert trigger's
    -- compare-and-swap needs a replace, not a create.
    rec.modified_by := p_modified_by;
    rec.performed_by := p_performed_by;
    rec.change_reason_code := p_reason_code;
    rec.change_commentary := format('Bumped by child %s: %s', p_child_entity, coalesce(p_commentary, ''));

    insert into "ores_reporting_engine_settings_tbl"
    select (rec).*;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace function ores_reporting_engine_settings_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate configuration_id (soft FK to ores_reporting_configurations_tbl)
    if not exists (
        select 1 from ores_reporting_configurations_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.configuration_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid configuration_id: %. No active configuration found with this id.', NEW.configuration_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_reporting_engine_settings_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        -- The write states what it believes about the row, and the store is
        -- what decides. Version zero means one thing: no current row exists.
        -- So a create that collides with a live row is refused here, for every
        -- client, rather than by a check each client has to remember.
        if NEW.version = 0 then
            if not ores_utility_version_replace_allowed_fn() then
                perform ores_outcome_raise_fn(
                    'already_exists',
                    'engine_settings',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'engine_settings',
                'id',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_reporting_engine_settings_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_reporting_engine_settings_insert_trg
before insert on "ores_reporting_engine_settings_tbl"
for each row execute function ores_reporting_engine_settings_insert_fn();

create or replace rule ores_reporting_engine_settings_delete_rule as
on delete to "ores_reporting_engine_settings_tbl" do instead (
    update "ores_reporting_engine_settings_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Engine Settings
-- =============================================================================
alter table ores_reporting_engine_settings_tbl enable row level security;

drop policy if exists engine_settings_tbl_tenant_isolation_policy
    on ores_reporting_engine_settings_tbl;

create policy engine_settings_tbl_tenant_isolation_policy
on ores_reporting_engine_settings_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation (RESTRICTIVE): ANDed with the permissive tenant
-- policy above, a session sees only rows whose party_id its visible
-- party set admits. The visible_party_ids-is-null passthrough applies
-- for sessions with no party restriction (tenant admins, service
-- contexts).
--
-- This table's rows belong to the account, not to one act of the session:
-- an account that works in several parties sees every party's rows in one
-- list. The actor's party set is what ores_iam_actor_party_ids_fn answers,
-- which is the account's own parties and nothing else.
drop policy if exists engine_settings_tbl_party_isolation_policy
    on ores_reporting_engine_settings_tbl;

create policy engine_settings_tbl_party_isolation_policy
on ores_reporting_engine_settings_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
    or party_id = ANY(ores_iam_actor_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
    or party_id = ANY(ores_iam_actor_party_ids_fn())
);
