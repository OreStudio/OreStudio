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
 * Sandbox Table
 *
 * A sandbox is a space where actions have no effect outside it: users import
 * samples, try what-ifs and run reports there without touching official data
 * (see [[id:4AB0BC63-D73A-4FC3-B9AF-16C1BB90653F][Sandbox]]).
 *
 * The anchor is the node of the official [[id:282C87C1-11E1-42F0-BEE6-D6983A5F836B][portfolio]] tree the sandbox
 * belongs to. It decides who may open the sandbox and who may see it; it
 * does not place the sandbox in the official tree. The sandbox's own
 * portfolios carry its id, and a portfolio always shares its parent's
 * sandbox, so no official portfolio contains a sandbox portfolio.
 *
 * Opening a sandbox needs the open_sandbox [[id:F783F38A-C123-489A-881D-5BCB5DE6D05D][portfolio right]] at
 * an official anchor; the check runs again when the owner changes.
 * ores_refdata_account_sees_sandbox_fn answers whether an account may see a
 * sandbox: its owner always may; anyone with read at the anchor may when it
 * is shared; its [[id:6B3A0A06-EE24-411F-BF99-BBEF04A43E06][members]] may when it is shared with members. A restrictive
 * row-level security policy on portfolios applies it to the session's actor
 * through ores_refdata_actor_sees_sandbox_fn, so a sandbox's portfolios are
 * read and written only by those who may see the sandbox, and a session with
 * no actor sees none of them.
 *
 * Closing a sandbox does not close its portfolios, as for every soft foreign
 * key in the schema. Handing a sandbox to a new owner needs the right at the
 * anchor on the new owner's side; the new owner's consent is not recorded.
 */

create table if not exists "ores_refdata_sandboxes_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "purpose" text not null,
    "anchor_portfolio_id" uuid not null,
    "owner_account_id" uuid not null,
    "visibility" text not null,
    "status" text not null,
    "review_date" date not null,
    "description" text null,
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
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("purpose" in ('experiment', 'sample', 'import')),
    check ("visibility" in ('private', 'shared', 'members')),
    check ("status" in ('open', 'archived'))
);

-- Unique name for active records
create unique index if not exists sandboxes_name_uniq_idx
on "ores_refdata_sandboxes_tbl" (tenant_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists sandboxes_version_uniq_idx
on "ores_refdata_sandboxes_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists sandboxes_id_uniq_idx
on "ores_refdata_sandboxes_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists sandboxes_tenant_idx
on "ores_refdata_sandboxes_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_sandboxes_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate anchor_portfolio_id (soft FK to ores_refdata_portfolios_tbl)
    if not exists (
        select 1 from ores_refdata_portfolios_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.anchor_portfolio_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid anchor_portfolio_id: %. No active portfolio found with this id.', NEW.anchor_portfolio_id
            using errcode = '23503';
    end if;

    -- Validate owner_account_id (soft FK to ores_iam_accounts_tbl)
    if not exists (
        select 1 from ores_iam_accounts_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.owner_account_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid owner_account_id: %. No account found with this id.', NEW.owner_account_id
            using errcode = '23503';
    end if;

    -- The anchor is a node of the official tree, never a sandbox portfolio.
    if exists (
        select 1 from ores_refdata_portfolios_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.anchor_portfolio_id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and sandbox_id is not null
    ) then
        raise exception 'Invalid anchor_portfolio_id: %. A sandbox is anchored at an official portfolio.',
            NEW.anchor_portfolio_id
            using errcode = '23514';
    end if;

    -- Opening a sandbox, or handing it to a new owner, needs the right to
    -- open sandboxes at the anchor. A version that keeps the owner, such as
    -- archiving, does not ask again.
    if not exists (
        select 1 from ores_refdata_sandboxes_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and owner_account_id = NEW.owner_account_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) and not ores_refdata_account_holds_portfolio_right_fn(
        NEW.tenant_id, NEW.owner_account_id, NEW.anchor_portfolio_id, 'open_sandbox'
    ) then
        raise exception 'Account % may not open a sandbox at portfolio %: it holds no open_sandbox right there.',
            NEW.owner_account_id, NEW.anchor_portfolio_id
            using errcode = '42501';
    end if;
    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_refdata_sandboxes_tbl"
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
        if exists (
            select 1 from "ores_refdata_sandboxes_tbl"
            where tenant_id = NEW.tenant_id
              and id = NEW.id
              and valid_to = ores_utility_infinity_timestamp_fn()
              and "anchor_portfolio_id" is distinct from NEW."anchor_portfolio_id"
        ) then
            raise exception 'anchor_portfolio_id cannot change: it is fixed for the life of the sandbox.'
                using errcode = '23514';
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_refdata_sandboxes_tbl"
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

create or replace trigger ores_refdata_sandboxes_insert_trg
before insert on "ores_refdata_sandboxes_tbl"
for each row execute function ores_refdata_sandboxes_insert_fn();

create or replace rule ores_refdata_sandboxes_delete_rule as
on delete to "ores_refdata_sandboxes_tbl" do instead (
    update "ores_refdata_sandboxes_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
-- Whether an account may see a sandbox: its owner always may; anyone with
-- read at the anchor may when it is shared; its members may when it is
-- shared with members. Runs with the caller's rights, so row-level security
-- confines it to the session's tenant. PL/pgSQL, so the members table, which
-- is created after this one, is resolved when the function runs.
create or replace function ores_refdata_account_sees_sandbox_fn(
    p_tenant_id uuid,
    p_account_id uuid,
    p_sandbox_id uuid
) returns boolean as $$
declare
    v_sandbox record;
begin
    select owner_account_id, anchor_portfolio_id, visibility into v_sandbox
    from ores_refdata_sandboxes_tbl
    where tenant_id = p_tenant_id
      and id = p_sandbox_id
      and valid_to = ores_utility_infinity_timestamp_fn();
    if not found then
        return false;
    end if;
    if v_sandbox.owner_account_id = p_account_id then
        return true;
    end if;
    if v_sandbox.visibility = 'shared' then
        return ores_refdata_account_holds_portfolio_right_fn(
            p_tenant_id, p_account_id, v_sandbox.anchor_portfolio_id, 'read');
    end if;
    if v_sandbox.visibility = 'members' then
        return exists (
            select 1 from ores_refdata_sandbox_members_tbl
            where tenant_id = p_tenant_id
              and sandbox_id = p_sandbox_id
              and account_id = p_account_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        );
    end if;
    return false;
end;
$$ language plpgsql stable set search_path = public, pg_temp;

-- The account of the user the session acts for, from app.current_actor.
-- Security definer and narrow, for the same reason as the liveness check:
-- it answers one id, for the session's own actor.
create or replace function ores_refdata_actor_account_id_fn(
    p_tenant_id uuid
) returns uuid as $$
declare
    v_id uuid;
begin
    select id into v_id
    from ores_iam_accounts_tbl
    where username = ores_iam_current_actor_fn()
      and tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
      and valid_to = ores_utility_infinity_timestamp_fn()
    order by (tenant_id = p_tenant_id) desc
    limit 1;
    return v_id;
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether the session's actor may see a sandbox. A session with no actor,
-- such as a system process, sees none. The portfolio policy uses it to keep
-- a sandbox's portfolios out of every read and write by anyone else.
create or replace function ores_refdata_actor_sees_sandbox_fn(
    p_tenant_id uuid,
    p_sandbox_id uuid
) returns boolean as $$
declare
    v_account uuid := ores_refdata_actor_account_id_fn(p_tenant_id);
begin
    return v_account is not null
       and ores_refdata_account_sees_sandbox_fn(p_tenant_id, v_account, p_sandbox_id);
end;
$$ language plpgsql stable set search_path = public, pg_temp;

-- =============================================================================
-- Row-level security: tenant isolation for Sandbox
-- =============================================================================
alter table ores_refdata_sandboxes_tbl enable row level security;

drop policy if exists sandboxes_tbl_tenant_isolation_policy
    on ores_refdata_sandboxes_tbl;

create policy sandboxes_tbl_tenant_isolation_policy
on ores_refdata_sandboxes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
