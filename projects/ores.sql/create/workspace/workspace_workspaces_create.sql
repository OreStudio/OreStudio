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
 * Workspace Table
 *
 * A named, isolated data context. Data a workspace does not carry is
 * resolved from its parent chain, up to the Live workspace
 * (ores_utility_live_workspace_id_fn()).
 *
 * The table is bi-temporal and audited (see
 * projects/ores.sql/create/workspace/workspace_create.sql): it carries
 * version, the four audit columns and the valid_from/valid_to pair
 * with the GIST exclusion and the delete rule, so the model takes the
 * ordinary audited shape and needs no shape flag.
 *
 * A workspace belongs to one tenant and one party, and its name is unique
 * within that pair. party_id is a natural key beside name for that
 * reason: two parties in one tenant may each hold a workspace named prod.
 * Only an active workspace holds its name, so the uniqueness index covers
 * the active rows and an archived name is free to be taken again.
 *
 * parent_workspace_id is a self-referencing soft foreign key, so the model
 * binds self-referencing-hierarchy for its UUID surrogate key, its tenant
 * scope and its self-reference. The same profile generates the recursive
 * subtree read over parent_workspace_id.
 *
 * scope_portfolio_id optionally narrows a workspace to one portfolio. It is
 * a soft foreign key with no declared target, because ores.refdata is
 * created after this component.
 *
 * The component's row-level security stays hand-written in
 * create/workspace/workspace_rls_policies_create.sql: the live fleet creates
 * this table before the iam section that defines
 * ores_iam_current_tenant_id_fn, so an inline policy cannot be emitted here.
 */

create table if not exists "ores_workspaces_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "party_id" uuid not null,
    "owner_id" uuid not null,
    "description" text not null default '',
    "source_path" text null,
    "parent_workspace_id" uuid null,
    "scope_portfolio_id" uuid null,
    "status_code" text not null default 'active',
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
    check ("name" <> ''),
    check ("status_code" in ('active', 'archived')),
    check ("id" <> "parent_workspace_id")
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists workspaces_version_uniq_idx
on "ores_workspaces_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists workspaces_id_uniq_idx
on "ores_workspaces_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists workspaces_tenant_idx
on "ores_workspaces_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists workspaces_name_uniq_idx
on "ores_workspaces_tbl" (tenant_id, party_id, name)
where valid_to = ores_utility_infinity_timestamp_fn()
  and "status_code" = 'active';

create index if not exists workspaces_party_idx
on "ores_workspaces_tbl" (party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists workspaces_status_idx
on "ores_workspaces_tbl" (status_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists workspaces_owner_idx
on "ores_workspaces_tbl" (owner_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_workspaces_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate party_id (soft FK to ores_refdata_parties_tbl)
    if not exists (
        select 1 from ores_refdata_parties_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid party_id: %. No active party found with this id.', NEW.party_id
            using errcode = '23503';
    end if;

    -- Validate parent_workspace_id (optional soft FK to ores_workspaces_tbl)
    if NEW.parent_workspace_id is not null then
        if not exists (
            select 1 from ores_workspaces_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.parent_workspace_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid parent_workspace_id: %. No active workspace found with this id.', NEW.parent_workspace_id
                using errcode = '23503';
        end if;

        -- Reject a parent_workspace_id pointing back at NEW's own row, directly or
        -- transitively, since the touch-version mechanism re-fires this
        -- same trigger walking up parent_workspace_id -- an undetected cycle would
        -- recurse without bound instead of failing cleanly.
        if exists (
            with recursive ancestor_chain as (
                select id, parent_workspace_id as parent_id
                from ores_workspaces_tbl
                where id = NEW.parent_workspace_id
                  and valid_to = ores_utility_infinity_timestamp_fn()
                union all
                select t.id, t.parent_workspace_id as parent_id
                from ores_workspaces_tbl t
                join ancestor_chain a on t.id = a.parent_id
                where t.valid_to = ores_utility_infinity_timestamp_fn()
            )
            select 1 from ancestor_chain where id = NEW.id
        ) then
            raise exception 'Invalid parent_workspace_id: % would create a cycle in the ores_workspaces_tbl hierarchy.', NEW.parent_workspace_id
                using errcode = '23514';
        end if;
    end if;

    -- Validate owner_id (soft FK to ores_iam_accounts_tbl)
    if not exists (
        select 1 from ores_iam_accounts_tbl
        where id = NEW.owner_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid owner_id: %. Must reference a valid account.', NEW.owner_id
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
    from "ores_workspaces_tbl"
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
        update "ores_workspaces_tbl"
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

create or replace trigger ores_workspaces_insert_trg
before insert on "ores_workspaces_tbl"
for each row execute function ores_workspaces_insert_fn();

create or replace rule ores_workspaces_delete_rule as
on delete to "ores_workspaces_tbl" do instead (
    update "ores_workspaces_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Hierarchy traversal for workspaces.
-- Returns a flat set of {id, parent_id, name} nodes for the subtree rooted
-- at p_root_id. When p_from_root is true, first walks up parent_id to the
-- ultimate ancestor (parent_id is null) and recurses down from there instead,
-- returning the whole tenant tree the given node belongs to.
-- =============================================================================
create or replace function ores_workspaces_hierarchy_fn(
    p_tenant_id uuid,
    p_root_id uuid,
    p_from_root boolean default false
) returns table(id uuid, parent_id uuid, name text) as $$
declare
    v_root_id uuid;
begin
    v_root_id := p_root_id;

    if p_from_root then
        with recursive ancestors as (
            select t.id, t.parent_workspace_id as parent_id,
                   array[t.id] as path
            from "ores_workspaces_tbl" t
            where t.tenant_id = p_tenant_id
              and t.id = p_root_id
              and t.valid_to = ores_utility_infinity_timestamp_fn()
            union all
            select t.id, t.parent_workspace_id as parent_id,
                   a.path || t.id
            from "ores_workspaces_tbl" t
            join ancestors a on t.id = a.parent_id
            where t.tenant_id = p_tenant_id
              and t.valid_to = ores_utility_infinity_timestamp_fn()
              -- A parent cycle walks forever; stop when a row repeats.
              and not t.id = any(a.path)
        )
        select a.id into v_root_id
        from ancestors a
        where a.parent_id is null
        limit 1;

        if v_root_id is null then
            v_root_id := p_root_id;
        end if;
    end if;

    return query
    with recursive descendants as (
        select t.id, t.parent_workspace_id as parent_id,
               t.name::text as name,
               array[t.id] as path
        from "ores_workspaces_tbl" t
        where t.tenant_id = p_tenant_id
          and t.id = v_root_id
          and t.valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select t.id, t.parent_workspace_id as parent_id,
               t.name::text as name,
               d.path || t.id
        from "ores_workspaces_tbl" t
        join descendants d on t.parent_workspace_id = d.id
        where t.tenant_id = p_tenant_id
          and t.valid_to = ores_utility_infinity_timestamp_fn()
          -- A parent cycle walks forever; stop when a row repeats.
          and not t.id = any(d.path)
    )
    select d.id, d.parent_id, d.name from descendants d;
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;
