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
 * The badge catalogue. Each entry defines the complete visual presentation
 * of a badge: display label, tooltip, background colour, text colour,
 * severity level, and an optional Bootstrap CSS class hint for Wt.
 *
 * Badge definitions are the single source of truth for all badge visual
 * metadata across Qt and Wt. They are loaded at client startup as
 * reference data and looked up at render time via BadgeCache.
 */

create table if not exists "ores_dq_badge_definitions_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "description" text not null,
    "background_colour" text not null,
    "text_colour" text not null,
    "severity_code" text not null,
    "css_class" text null,
    "display_order" integer not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, code, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("code" <> '')
);

-- Unique name for active records
create unique index if not exists ores_dq_badge_definitions_name_uniq_idx
on "ores_dq_badge_definitions_tbl" (tenant_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists ores_dq_badge_definitions_version_uniq_idx
on "ores_dq_badge_definitions_tbl" (tenant_id, code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_dq_badge_definitions_id_uniq_idx
on "ores_dq_badge_definitions_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_dq_badge_definitions_tenant_idx
on "ores_dq_badge_definitions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_dq_badge_definitions_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Version management
    select version into current_version
    from "ores_dq_badge_definitions_tbl"
    where tenant_id = NEW.tenant_id
      and code = NEW.code
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        update "ores_dq_badge_definitions_tbl"
        set valid_to = current_timestamp
        where tenant_id = NEW.tenant_id
          and code = NEW.code
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_actor_fn(), current_user);

    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_dq_badge_definitions_insert_trg
before insert on "ores_dq_badge_definitions_tbl"
for each row execute function ores_dq_badge_definitions_insert_fn();

create or replace rule ores_dq_badge_definitions_delete_rule as
on delete to "ores_dq_badge_definitions_tbl" do instead
    update "ores_dq_badge_definitions_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and code = OLD.code
      and valid_to = ores_utility_infinity_timestamp_fn();
