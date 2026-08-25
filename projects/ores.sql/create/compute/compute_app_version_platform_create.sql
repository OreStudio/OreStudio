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
 * Template: sql_schema_junction_create.mustache
 * To modify, update the template and regenerate.
 *
 * App Version Platform Table
 *
 * Associates app versions with the platforms they support, and carries the URI
 * of the per-platform packaged bundle (a .tar.gz containing the wrapper+engine
 * binaries built for that target triplet). Each (app_version, platform) row
 * owns its own package_uri, so the orchestrator can dispatch per-triplet
 * assignments without the wrapper needing to negotiate package selection.
 *
 * The junction uses the full generated stack: NATS protocol, handler
 * and registrar. The generated list_by_app_version op returns the
 * platform rows enriched with the platform code, and
 * replace_by_app_version replaces the active platform set of an app
 * version. Both are consumed by the desktop console and by the
 * repository tests.
 */

create table if not exists "ores_compute_app_version_platforms_tbl" (
    "app_version_id" uuid not null,
    "tenant_id" uuid not null,
    "platform_id" uuid not null,
    "version" integer not null,
    "package_uri" text not null,
    "sha256" text not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, app_version_id, platform_id, valid_from),
    exclude using gist (
        tenant_id WITH =,
        app_version_id WITH =,
        platform_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Index for looking up platforms for an app version
create index if not exists app_version_platforms_app_version_idx
on "ores_compute_app_version_platforms_tbl" (app_version_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Index for finding app versions supporting a platform
create index if not exists app_version_platforms_platform_idx
on "ores_compute_app_version_platforms_tbl" (platform_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique constraint on active records for ON CONFLICT support
create unique index if not exists app_version_platforms_uniq_idx
on "ores_compute_app_version_platforms_tbl" (tenant_id, app_version_id, platform_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists app_version_platforms_tenant_idx
on "ores_compute_app_version_platforms_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_compute_app_version_platforms_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- Version management
    select version into current_version
    from "ores_compute_app_version_platforms_tbl"
    where tenant_id = new.tenant_id
    and app_version_id = new.app_version_id
    and platform_id = new.platform_id
    and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                new.version, current_version
                using errcode = 'P0002';
        end if;
        new.version = current_version + 1;

        -- Close existing record
        update "ores_compute_app_version_platforms_tbl"
        set valid_to = current_timestamp
        where tenant_id = new.tenant_id
        and app_version_id = new.app_version_id
        and platform_id = new.platform_id
        and valid_to = ores_utility_infinity_timestamp_fn()
        and valid_from < current_timestamp;
    else
        new.version = 1;
    end if;

    new.valid_from = current_timestamp;
    new.valid_to = ores_utility_infinity_timestamp_fn();

    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_compute_app_version_platforms_insert_trg
before insert on "ores_compute_app_version_platforms_tbl"
for each row
execute function ores_compute_app_version_platforms_insert_fn();

create or replace rule ores_compute_app_version_platforms_delete_rule as
on delete to "ores_compute_app_version_platforms_tbl"
do instead
  update "ores_compute_app_version_platforms_tbl"
  set valid_to = current_timestamp
  where tenant_id = old.tenant_id
  and app_version_id = old.app_version_id
  and platform_id = old.platform_id
  and valid_to = ores_utility_infinity_timestamp_fn();
