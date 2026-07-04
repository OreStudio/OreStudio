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
 * Counterparty Table
 *
 * External trading partners and counterparties that participate in financial
 * transactions with the organisation. Counterparties form a hierarchy through
 * parent_counterparty_id for group structures.
 */

create table if not exists "ores_refdata_counterparties_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "short_code" text not null,
    "full_name" text not null,
    "transliterated_name" text null,
    "party_type" text not null,
    "parent_counterparty_id" uuid null,
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
    check ("id" <> ores_utility_nil_uuid_fn())
);

-- Unique short_code for active records
create unique index if not exists counterparties_short_code_uniq_idx
on "ores_refdata_counterparties_tbl" (tenant_id, short_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists counterparties_version_uniq_idx
on "ores_refdata_counterparties_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists counterparties_id_uniq_idx
on "ores_refdata_counterparties_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists counterparties_tenant_idx
on "ores_refdata_counterparties_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists counterparties_full_name_idx
on "ores_refdata_counterparties_tbl" (tenant_id, full_name)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_counterparties_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate parent_counterparty_id (optional soft FK to ores_refdata_counterparties_tbl)
    if NEW.parent_counterparty_id is not null then
        if not exists (
            select 1 from ores_refdata_counterparties_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.parent_counterparty_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid parent_counterparty_id: %. No active counterparty found with this id.', NEW.parent_counterparty_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate party_type
    NEW.party_type := ores_refdata_validate_party_type_fn(NEW.tenant_id, NEW.party_type);

    -- Validate status
    NEW.status := ores_refdata_validate_party_status_fn(NEW.tenant_id, NEW.status);

    -- Validate business_center_code
    NEW.business_center_code := ores_refdata_validate_business_centre_fn(NEW.tenant_id, NEW.business_center_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_refdata_counterparties_tbl"
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

        update "ores_refdata_counterparties_tbl"
        set valid_to = current_timestamp
        where tenant_id = NEW.tenant_id
          and id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_refdata_counterparties_insert_trg
before insert on "ores_refdata_counterparties_tbl"
for each row execute function ores_refdata_counterparties_insert_fn();

create or replace rule ores_refdata_counterparties_delete_rule as
on delete to "ores_refdata_counterparties_tbl" do instead
    update "ores_refdata_counterparties_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Hierarchy traversal for counterparties.
-- Returns a flat set of {id, parent_id, name} nodes for the subtree rooted
-- at p_root_id. When p_from_root is true, first walks up parent_id to the
-- ultimate ancestor (parent_id is null) and recurses down from there instead,
-- returning the whole tenant tree the given node belongs to.
-- =============================================================================
create or replace function ores_refdata_counterparties_hierarchy_fn(
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
            select t.id, t.parent_counterparty_id as parent_id
            from "ores_refdata_counterparties_tbl" t
            where t.tenant_id = p_tenant_id
              and t.id = p_root_id
              and t.valid_to = ores_utility_infinity_timestamp_fn()
            union all
            select t.id, t.parent_counterparty_id as parent_id
            from "ores_refdata_counterparties_tbl" t
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
        select t.id, t.parent_counterparty_id as parent_id,
               t.full_name::text as name
        from "ores_refdata_counterparties_tbl" t
        where t.tenant_id = p_tenant_id
          and t.id = v_root_id
          and t.valid_to = ores_utility_infinity_timestamp_fn()
        union all
        select t.id, t.parent_counterparty_id as parent_id,
               t.full_name::text as name
        from "ores_refdata_counterparties_tbl" t
        join descendants d on t.parent_counterparty_id = d.id
        where t.tenant_id = p_tenant_id
          and t.valid_to = ores_utility_infinity_timestamp_fn()
    )
    select d.id, d.parent_id, d.name from descendants d;
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;
