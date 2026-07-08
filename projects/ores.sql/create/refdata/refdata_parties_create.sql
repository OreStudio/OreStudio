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
 *  Table
 *
 * Internal legal entities (the organisation and its subsidiaries) that participate
 * in financial transactions. Parties form a hierarchy through parent_party_id,
 * with exactly one root party per tenant representing the organisation itself.
 */

create table if not exists "ores_refdata_parties_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "full_name" text not null,
    "short_code" text not null,
    "codename" text not null default '',
    "transliterated_name" text null,
    "party_category" text not null default 'Operational',
    "party_type" text not null,
    "parent_party_id" uuid null,
    "business_center_code" text not null,
    "status" text not null default 'Active',
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
    check (length("codename") <= 32),
    check ("codename" = '' or "codename" ~ '^[a-z][a-z_]+$')
);

-- Non-unique full_name index for search. Full names are not unique in
-- real-world data (e.g. "CAISSE LOCALE CREDIT AGRICOLE" appears for
-- multiple branches, each with a distinct LEI).
create index if not exists parties_full_name_idx
on "ores_refdata_parties_tbl" (tenant_id, full_name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique short_code for active records
create unique index if not exists parties_short_code_uniq_idx
on "ores_refdata_parties_tbl" (tenant_id, short_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists parties_version_uniq_idx
on "ores_refdata_parties_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists parties_id_uniq_idx
on "ores_refdata_parties_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists parties_tenant_idx
on "ores_refdata_parties_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Root party uniqueness: exactly one operational root party per tenant.
-- The system party (party_category='System') is excluded — it has its
-- own uniqueness constraint below.
create unique index if not exists parties_root_party_uniq_idx
on "ores_refdata_parties_tbl" (tenant_id)
where parent_party_id is null and party_category <> 'System' and valid_to = ores_utility_infinity_timestamp_fn();

-- System party uniqueness: exactly one system party per tenant
create unique index if not exists parties_system_party_uniq_idx
on "ores_refdata_parties_tbl" (tenant_id)
where party_category = 'System' and valid_to = ores_utility_infinity_timestamp_fn();

-- Globally unique codename across all tenants (empty string excluded)
create unique index if not exists parties_codename_uniq_idx
on "ores_refdata_parties_tbl" (codename)
where valid_to = ores_utility_infinity_timestamp_fn() and codename <> '';

-- Sequence used to generate a unique base-26 suffix for auto-generated
-- codenames.  nextval() is outside MVCC: it advances even within a
-- multi-row INSERT statement, so every row in a batch gets a different
-- suffix, avoiding duplicate-key collisions that a NOT EXISTS loop cannot
-- prevent due to the statement-level snapshot in READ COMMITTED.
create sequence if not exists ores_refdata_party_codename_seq;

create or replace function ores_refdata_parties_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate party_category
    NEW.party_category := ores_refdata_validate_party_category_fn(NEW.tenant_id, NEW.party_category);

    -- Validate party_type
    NEW.party_type := ores_refdata_validate_party_type_fn(NEW.tenant_id, NEW.party_type);

    -- Validate status
    NEW.status := ores_refdata_validate_party_status_fn(NEW.tenant_id, NEW.status);

    -- Validate parent_party_id (soft FK)
    if NEW.parent_party_id is not null then
        if not exists (
            select 1 from ores_refdata_parties_tbl
            where tenant_id = NEW.tenant_id and id = NEW.parent_party_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid parent_party_id: %. No active party found with this id.',
                NEW.parent_party_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate business_center_code (mandatory)
    if NEW.business_center_code is null or NEW.business_center_code = '' then
        raise exception 'business_center_code is required for parties'
            using errcode = '23502';
    end if;
    NEW.business_center_code := ores_refdata_validate_business_centre_fn(
        NEW.tenant_id, NEW.business_center_code);

    -- Version management
    select version into current_version
    from "ores_refdata_parties_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        -- Codename is immutable: restore from active row on every update.
        select codename into NEW.codename
        from "ores_refdata_parties_tbl"
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn();

        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_refdata_parties_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;

        -- Auto-generate codename using whimsical name + sequence suffix.
        -- The sequence suffix is outside MVCC (nextval always advances),
        -- so every row in a multi-row INSERT gets a distinct suffix, making
        -- the codename globally unique without relying on a NOT EXISTS
        -- check that cannot see sibling rows in the same statement.
        if NEW.codename = '' or NEW.codename is null then
            NEW.codename := ores_utility_generate_whimsical_name_fn() || '_' ||
                ores_utility_to_base26_fn(nextval('ores_refdata_party_codename_seq'));
        end if;
        -- Validate the final codename.
        if NEW.codename !~ '^[a-z][a-z_]+$' then
            raise exception 'codename must match ^[a-z][a-z_]+$ got: %', NEW.codename
                using errcode = '23514';
        end if;
        -- Provision the per-party report event queue.
        perform ores_mq_queues_create_fn(
            NEW.tenant_id, NEW.id, 'party', 'task',
            'report_events', 'Per-party report scheduling queue', current_user);
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();

    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_refdata_parties_insert_trg
before insert on "ores_refdata_parties_tbl"
for each row execute function ores_refdata_parties_insert_fn();

create or replace rule ores_refdata_parties_delete_rule as
on delete to "ores_refdata_parties_tbl" do instead
    update "ores_refdata_parties_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Touch-version function for party
-- Bumps this entity's version without changing any of its own columns,
-- called by a child entity's insert trigger / delete rule when that
-- child is flagged :bump_parent_version: true against this entity. See
-- the "Temporal composite entity versioning" architecture doc.
--
-- Deliberately does NOT duplicate the version/valid_from/valid_to
-- management here: it fetches the current row, resets version to the
-- 0 sentinel, and re-inserts — the table's own insert trigger (above)
-- then performs the exact same close-current/bump-version dance a
-- normal application save does.
--
-- p_reason_code is passed through as-is (already a validated code from
-- the child's own row); p_child_entity distinguishes which child
-- triggered the bump in the free-text commentary.
-- =============================================================================
create or replace function ores_refdata_parties_touch_version_fn(
    p_tenant_id uuid,
    p_id uuid,
    p_reason_code text,
    p_commentary text,
    p_modified_by text,
    p_performed_by text,
    p_child_entity text
) returns void as $$
declare
    rec ores_refdata_parties_tbl%rowtype;
begin
    -- for update: takes the same row lock the parent's own insert
    -- trigger takes, so the snapshot in rec can't be based on a
    -- business-column value a concurrent direct edit is about to
    -- change — without this, that concurrent edit could be silently
    -- reverted once this function's later insert proceeds. See the
    -- "Temporal composite entity versioning" architecture doc,
    -- Concurrency section.
    select * into rec
    from "ores_refdata_parties_tbl"
    where tenant_id = p_tenant_id
      and id = p_id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if not found then
        return;
    end if;

    rec.version := 0;
    rec.modified_by := p_modified_by;
    rec.performed_by := p_performed_by;
    rec.change_reason_code := p_reason_code;
    rec.change_commentary := format('Bumped by child %s: %s', p_child_entity, coalesce(p_commentary, ''));

    insert into "ores_refdata_parties_tbl"
    select (rec).*;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Hierarchy traversal for parties.
-- Returns a flat set of {id, parent_id, name} nodes for the subtree rooted
-- at p_root_id. When p_from_root is true, first walks up parent_id to the
-- ultimate ancestor (parent_id is null) and recurses down from there instead,
-- returning the whole tenant tree the given node belongs to.
-- =============================================================================
create or replace function ores_refdata_parties_hierarchy_fn(
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
            select t.id, t.parent_party_id as parent_id
            from "ores_refdata_parties_tbl" t
            where t.tenant_id = p_tenant_id
              and t.id = p_root_id
              and t.valid_to = ores_utility_infinity_timestamp_fn()
            union all
            select t.id, t.parent_party_id as parent_id
            from "ores_refdata_parties_tbl" t
            join ancestors a on t.id = a.parent_id
            where t.tenant_id = p_tenant_id
              and t.valid_to = ores_utility_infinity_timestamp_fn()
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
        select t.id, t.parent_party_id as parent_id,
               t.full_name::text as name
        from "ores_refdata_parties_tbl" t
        where t.tenant_id = p_tenant_id
          and t.id = v_root_id
          and t.valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select t.id, t.parent_party_id as parent_id,
               t.full_name::text as name
        from "ores_refdata_parties_tbl" t
        join descendants d on t.parent_party_id = d.id
        where t.tenant_id = p_tenant_id
          and t.valid_to = ores_utility_infinity_timestamp_fn()
    )
    select d.id, d.parent_id, d.name from descendants d;
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;
