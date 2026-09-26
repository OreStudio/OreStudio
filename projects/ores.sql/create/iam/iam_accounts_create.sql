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
 * Account Table
 *
 * An account that can authenticate against the system: one row per user,
 * service, algorithm or LLM identity, carrying the password material, the
 * TOTP secret, the email address and the optional profile and reporting
 * links. The table is bi-temporal and audited (see
 * projects/ores.sql/create/iam/iam_accounts_create.sql): it carries
 * version, the four audit columns and the valid_from/valid_to pair
 * with the GIST exclusion, so the model takes the ordinary audited shape
 * and needs no shape flag.
 *
 * The table is a composite parent: ores_iam_accounts_touch_version_fn
 * lets a child entity (account contact information, party association)
 * bump this account's own version when the child is written. The model
 * declares :generate_touch_function: true, which renders that function
 * under its existing name rather than leaving it hand-written.
 *
 * The model describes the table and nothing else. Two columns need care:
 *
 * - service_password_hash is a real column with no domain member: it is
 *   reached only by check_service_credentials and never travels on the
 *   wire, so it is declared :sql_only: true and the generated domain
 *   struct omits it while the entity struct and the mapper keep it.
 * - image_id and reports_to_account_id are nullable UUID soft
 *   references. The hand-written domain struct represented both as a plain
 *   boost::uuids::uuid with a nil sentinel, on the claim that a second
 *   std::optional<boost::uuids::uuid> member corrupts reflect-cpp
 *   aggregate serialisation for multi-element vectors. Re-verified under
 *   the generated estate: all three nullable UUIDs are modelled as
 *   std::optional<boost::uuids::uuid>, and the api suite's multi-element
 *   JSON and table tests plus the core repository's five-account round trip
 *   pass, so the workaround is not needed here.
 *
 * The generated read surface is live, and it does not collide with the
 * hand-written one. The hand-written
 * account_operations_protocol.hpp owns the writes,
 * iam.v1.accounts.{save,delete,update,lock,unlock,change-password,reset-password,select-party,set-default-party,switch-party,update-email,publish-from-dq},
 * and the generated account_protocol.hpp owns the reads,
 * iam.v1.accounts.list and iam.v1.accounts.get, with the version reads
 * alongside them. Both registrars are composed in
 * ores.iam/core/src/messaging/registrar.cpp. This entity sets
 * :read_only: true, so the generated half carries no write verb and the split
 * falls out of the flag rather than out of a suppression.
 *
 * Two behavioural facets are switched off, each with a reason:
 *
 * - The entity's CRUD handler and sub-registrar, because the hand-written
 *   account_operations_handler already owns the write verbs.
 * - The generated CRUD service, because the hand-written account_operations_service
 *   is the authentication surface (login, lock, unlock, password change
 *   and reset, party selection, service-credential check) and the
 *   generated service's get_account_history(id) collides in name and
 *   signature with the hand-written get_account_history(username) while
 *   meaning a different read.
 */

create table if not exists "ores_iam_accounts_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "username" text not null,
    "account_type" text not null default 'user',
    "full_name" text null,
    "password_hash" text not null,
    "password_salt" text not null,
    "service_password_hash" text null,
    "totp_secret" text not null,
    "email" text not null,
    "default_party_id" uuid null,
    "image_id" uuid null,
    "job_title" text null,
    "reports_to_account_id" uuid null,
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
    check ("valid_from" < "valid_to")
);

-- Unique username for active records
create unique index if not exists accounts_username_uniq_idx
on "ores_iam_accounts_tbl" (tenant_id, username)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists accounts_version_uniq_idx
on "ores_iam_accounts_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists accounts_id_uniq_idx
on "ores_iam_accounts_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists accounts_tenant_idx
on "ores_iam_accounts_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique email for active records
create unique index if not exists accounts_email_uniq_idx
on "ores_iam_accounts_tbl" (tenant_id, email)
where valid_to = ores_utility_infinity_timestamp_fn();


-- =============================================================================
-- Touch-version function for account
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
create or replace function ores_iam_accounts_touch_version_fn(
    p_tenant_id uuid,
    p_id uuid,
    p_reason_code text,
    p_commentary text,
    p_modified_by text,
    p_performed_by text,
    p_child_entity text
) returns void as $$
declare
    rec ores_iam_accounts_tbl%rowtype;
begin
    -- for update: takes the same row lock the parent's own insert
    -- trigger takes, so the snapshot in rec can't be based on a
    -- business-column value a concurrent direct edit is about to
    -- change — without this, that concurrent edit could be silently
    -- reverted once this function's later insert proceeds. See the
    -- "Temporal composite entity versioning" architecture doc,
    -- Concurrency section.
    select * into rec
    from "ores_iam_accounts_tbl"
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

    insert into "ores_iam_accounts_tbl"
    select (rec).*;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace function ores_iam_accounts_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate image_id (optional soft FK to ores_assets_images_tbl)
    if NEW.image_id is not null then
        if not exists (
            select 1 from ores_assets_images_tbl
            where tenant_id = NEW.tenant_id
              and image_id = NEW.image_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid image_id: %. Image must exist.', NEW.image_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate reports_to_account_id (optional soft FK to ores_iam_accounts_tbl)
    if NEW.reports_to_account_id is not null then
        if not exists (
            select 1 from ores_iam_accounts_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.reports_to_account_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid reports_to_account_id: %. Account must exist.', NEW.reports_to_account_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate account_type
    NEW.account_type := ores_iam_validate_account_type_fn(NEW.tenant_id, NEW.account_type);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_iam_accounts_tbl"
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
                raise exception
                    'Row already exists: a create cannot replace it. State the version you read to replace the row, or ask for a version replace.'
                    using errcode = '23505';
            end if;
        elsif NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_iam_accounts_tbl"
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
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_iam_accounts_insert_trg
before insert on "ores_iam_accounts_tbl"
for each row execute function ores_iam_accounts_insert_fn();

create or replace rule ores_iam_accounts_delete_rule as
on delete to "ores_iam_accounts_tbl" do instead (
    update "ores_iam_accounts_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
