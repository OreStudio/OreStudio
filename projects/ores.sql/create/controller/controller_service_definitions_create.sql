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
 * Represents a Kubernetes-style Deployment: the desired configuration for a
 * named service, including how many replicas to run, the binary name, restart
 * policy, and whether the service is enabled. This is a bitemporal table so
 * that the full history of configuration changes is preserved for audit.
 */

create table if not exists "ores_controller_service_definitions_tbl" (
    "id" uuid not null,
    "version" integer not null,
    "service_name" text not null,
    "binary_name" text not null,
    "desired_replicas" integer not null default 1,
    "restart_policy" text not null default 'always',
    "max_restart_count" integer not null default 3,
    "enabled" integer not null default 1,
    "args_template" text null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (id, valid_from, valid_to),
    exclude using gist (
        id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid)
);

-- Unique service_name for active records
create unique index if not exists ores_controller_service_definitions_service_name_uniq_idx
on "ores_controller_service_definitions_tbl" (service_name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists ores_controller_service_definitions_version_uniq_idx
on "ores_controller_service_definitions_tbl" (id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_controller_service_definitions_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Version management
    select version into current_version
    from "ores_controller_service_definitions_tbl"
    where id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        update "ores_controller_service_definitions_tbl"
        set valid_to = current_timestamp
        where id = NEW.id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_actor_fn(), current_user);

    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_controller_service_definitions_insert_trg
before insert on "ores_controller_service_definitions_tbl"
for each row execute function ores_controller_service_definitions_insert_fn();

create or replace rule ores_controller_service_definitions_delete_rule as
on delete to "ores_controller_service_definitions_tbl" do instead
    update "ores_controller_service_definitions_tbl"
    set valid_to = current_timestamp
    where id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
