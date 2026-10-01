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
 * Curve Definition Table
 *
 * One curve entry of one section of a curveconfig.xml. Seventy-six of those
 * documents ship and hold two thousand eight hundred and fifty-three entries
 * across nineteen sections.
 *
 * Every entry, whatever its section, opens the same way -- a CurveId and a
 * CurveDescription occur once per entry in the corpus, and the settings that
 * follow are drawn from a much smaller vocabulary than the sections suggest:
 * Currency in two thousand one hundred and ninety-two entries, DiscountCurve
 * in one thousand two hundred and eighty-nine, DayCounter in one thousand two
 * hundred and seventy-five, InterpolationMethod in seven hundred and
 * thirty-four. Those are columns. What is left over differs by section --
 * ImplyDefaultFromMarket and RecoveryRate belong to default curves,
 * ExerciseStyle to equity curves, BootstrapConfig to most of them -- and is
 * written into extras as a stated list of name and value pairs, so a section
 * the mapper has not read yet is carried rather than dropped.
 *
 * The lists an entry may hold -- Segments, Quotes, Pillars,
 * Configurations -- are ordered and heterogeneous and belong in child tables
 * that follow this one. This table is the entry's own settings.
 *
 * The natural key is the section and the curve id together: EUR-ESTR names a
 * curve inside one section, and nothing stops another section naming its own.
 */

create table if not exists "ores_refdata_curve_definitions_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "section_code" text not null,
    "curve_id" text not null,
    "description" text null,
    "currency" text null,
    "day_counter" text null,
    "interpolation_method" text null,
    "interpolation_variable" text null,
    "extrapolation" text null,
    "tolerance" text null,
    "extras" text null,
    "position" integer not null,
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
    check ("section_code" <> ''),
    check ("curve_id" <> '')
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists curve_definitions_version_uniq_idx
on "ores_refdata_curve_definitions_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists curve_definitions_id_uniq_idx
on "ores_refdata_curve_definitions_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists curve_definitions_tenant_idx
on "ores_refdata_curve_definitions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_curve_definitions_insert_fn()
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
    from "ores_refdata_curve_definitions_tbl"
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
        update "ores_refdata_curve_definitions_tbl"
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

create or replace trigger ores_refdata_curve_definitions_insert_trg
before insert on "ores_refdata_curve_definitions_tbl"
for each row execute function ores_refdata_curve_definitions_insert_fn();

create or replace rule ores_refdata_curve_definitions_delete_rule as
on delete to "ores_refdata_curve_definitions_tbl" do instead (
    update "ores_refdata_curve_definitions_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
