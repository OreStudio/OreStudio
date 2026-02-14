/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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

-- =============================================================================
-- Describes the origin of data.
-- Examples: vendor, internal, regulatory.
-- =============================================================================

create table if not exists "ores_dq_origin_dimensions_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "description" text not null,
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

create unique index if not exists ores_dq_origin_dimensions_version_uniq_idx
on "ores_dq_origin_dimensions_tbl" (tenant_id, code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_dq_origin_dimensions_code_uniq_idx
on "ores_dq_origin_dimensions_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_dq_origin_dimensions_tenant_idx
on "ores_dq_origin_dimensions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_dq_origin_dimensions_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    select version into current_version
    from "ores_dq_origin_dimensions_tbl"
    where tenant_id = NEW.tenant_id
      and code = NEW.code
      and valid_to = ores_utility_infinity_timestamp_fn();

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        update "ores_dq_origin_dimensions_tbl"
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

    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = current_user;

    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_dq_origin_dimensions_insert_trg
before insert on "ores_dq_origin_dimensions_tbl"
for each row execute function ores_dq_origin_dimensions_insert_fn();

create or replace rule ores_dq_origin_dimensions_delete_rule as
on delete to "ores_dq_origin_dimensions_tbl" do instead
    update "ores_dq_origin_dimensions_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and code = OLD.code
      and valid_to = ores_utility_infinity_timestamp_fn();
