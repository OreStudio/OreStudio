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
 * Series Key Shape Table
 *
 * The key grammar of ORE market data, one row per series type. Every ORE
 * key follows the skeleton TYPE/METRIC/[QUALIFIER...]/[POINT_ID], and
 * this table says where the split falls: qualifier_depth counts the
 * segments after the metric that identify the series and stay stable
 * across market dates, and every remaining segment is the point (a tenor,
 * a strike, a surface coordinate).
 *
 * The table belongs to ores.ore because the grammar it records is
 * ORE's, not ours: ORE defines the file format and we only read it. It
 * replaces a compiled C++ table, so a type ORE adds later, or one a user
 * brings, is an inserted row rather than a rebuild. A type with no row is
 * not an error -- its key folds whole into the qualifier and still
 * reconstructs verbatim -- so an uncatalogued type never aborts an
 * import.
 *
 * Two invariants hold. The reader rejects an empty table, because an
 * empty table silently degrades every key into a series of its own. A row
 * that claims a point dimension and also carries a default point is
 * contradictory and is rejected, both by the reader and by the check
 * constraint below.
 */

create table if not exists "ores_ore_series_key_shapes_tbl" (
    "series_type" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "qualifier_depth" integer not null,
    "has_point_dimension" boolean not null,
    "default_point" text not null default '',
    "description" text not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, series_type, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        series_type WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("series_type" <> ''),
    check (not ("has_point_dimension" and "default_point" <> ''))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists series_key_shapes_version_uniq_idx
on "ores_ore_series_key_shapes_tbl" (tenant_id, series_type, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists series_key_shapes_series_type_uniq_idx
on "ores_ore_series_key_shapes_tbl" (tenant_id, series_type)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists series_key_shapes_tenant_idx
on "ores_ore_series_key_shapes_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_ore_series_key_shapes_insert_fn()
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
    from "ores_ore_series_key_shapes_tbl"
    where tenant_id = NEW.tenant_id
      and series_type = NEW.series_type
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
        update "ores_ore_series_key_shapes_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and series_type = NEW.series_type
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

create or replace trigger ores_ore_series_key_shapes_insert_trg
before insert on "ores_ore_series_key_shapes_tbl"
for each row execute function ores_ore_series_key_shapes_insert_fn();

create or replace rule ores_ore_series_key_shapes_delete_rule as
on delete to "ores_ore_series_key_shapes_tbl" do instead (
    update "ores_ore_series_key_shapes_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and series_type = OLD.series_type
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Series Key Shape
-- =============================================================================
alter table ores_ore_series_key_shapes_tbl enable row level security;

create policy series_key_shapes_tbl_tenant_isolation_policy
on ores_ore_series_key_shapes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
