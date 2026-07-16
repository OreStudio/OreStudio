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
 * Folder Table
 *
 * A generic, self-referencing hierarchy node. The Market Simulator tree is
 * drawn directly from these rows rather than parsed out of a feed's
 * source_name string: one root folder per party ("Synthetic"), with
 * market_data_generation_config collections, asset-class folders (e.g.
 * "FX"), and instrument-type folders (e.g. "FX Rates") nested beneath it.
 * fx_spot_generation_config rows reference their immediate parent folder
 * via folder_id. kind distinguishes what a given row represents; =
 * collection_id= is populated only for kind  'collection'= rows, linking
 * back to the market_data_generation_config it represents (that entity
 * remains the source of truth for name/description/enabled/dataset_id;
 * this row's own name is kept in sync for uniform hierarchy display).
 *
 * Having real, queryable folder rows (rather than string-parsing) lets any
 * caller -- Qt, ores.shell, or a wt workflow -- resolve "everything under
 * this folder" the same way, via the generated hierarchy function, instead
 * of each reimplementing the tree-walk itself.
 */

create table if not exists "ores_synthetic_folders_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "name" text not null,
    "kind" text not null,
    "collection_id" uuid null,
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
    check ("kind" in ('root', 'collection', 'asset_class', 'instrument_type'))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists folders_version_uniq_idx
on "ores_synthetic_folders_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists folders_id_uniq_idx
on "ores_synthetic_folders_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists folders_tenant_idx
on "ores_synthetic_folders_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_synthetic_folders_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_synthetic_folders_tbl"
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

        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_synthetic_folders_tbl"
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

create or replace trigger ores_synthetic_folders_insert_trg
before insert on "ores_synthetic_folders_tbl"
for each row execute function ores_synthetic_folders_insert_fn();

create or replace rule ores_synthetic_folders_delete_rule as
on delete to "ores_synthetic_folders_tbl" do instead (
    update "ores_synthetic_folders_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Validation function for folder
-- Validates that a id exists in the folders table.
-- Returns the validated value, or default if null/empty.
-- Validates against both the tenant's own data and the system tenant's canonical set.
-- =============================================================================
create or replace function ores_synthetic_validate_folder_fn(
    p_tenant_id uuid,
    p_value uuid
) returns uuid as $$
begin
    -- Return default if null or empty
    if p_value is null then
        raise exception 'Invalid folder: value cannot be null'
            using errcode = '23502';
    end if;

    -- Allow pass-through if neither this tenant nor the system tenant has
    -- seeded active folders yet (freshly provisioned tenant).
    if not exists (
        select 1 from ores_synthetic_folders_tbl
        where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return p_value;
    end if;

    -- Validate against this tenant's values and the system tenant's canonical set.
    if not exists (
        select 1 from ores_synthetic_folders_tbl
        where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
          and id = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid folder: %. Must be one of: %', p_value, (
            select string_agg(id::text, ', ' order by id)
            from ores_synthetic_folders_tbl
            where tenant_id in (p_tenant_id, ores_utility_system_tenant_id_fn())
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

-- =============================================================================
-- Hierarchy traversal for folders.
-- Returns a flat set of {id, parent_id, name} nodes for the subtree rooted
-- at p_root_id. When p_from_root is true, first walks up parent_id to the
-- ultimate ancestor (parent_id is null) and recurses down from there instead,
-- returning the whole tenant tree the given node belongs to.
-- =============================================================================
create or replace function ores_synthetic_folders_hierarchy_fn(
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
            select t.id, t.parent_id as parent_id
            from "ores_synthetic_folders_tbl" t
            where t.tenant_id = p_tenant_id
              and t.id = p_root_id
              and t.valid_to = ores_utility_infinity_timestamp_fn()
            union all
            select t.id, t.parent_id as parent_id
            from "ores_synthetic_folders_tbl" t
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
        select t.id, t.parent_id as parent_id,
               t.name::text as name
        from "ores_synthetic_folders_tbl" t
        where t.tenant_id = p_tenant_id
          and t.id = v_root_id
          and t.valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select t.id, t.parent_id as parent_id,
               t.name::text as name
        from "ores_synthetic_folders_tbl" t
        join descendants d on t.parent_id = d.id
        where t.tenant_id = p_tenant_id
          and t.valid_to = ores_utility_infinity_timestamp_fn()
    )
    select d.id, d.parent_id, d.name from descendants d;
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;
