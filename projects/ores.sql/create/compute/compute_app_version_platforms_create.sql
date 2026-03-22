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

-- =============================================================================
-- Compute App Version Platforms (bitemporal junction table)
-- =============================================================================
-- Associates app versions with the platforms they support. Tenant-scoped:
-- each tenant manages which platform associations are active for their app
-- versions. Supports re-enabling a previously removed platform association.

create table if not exists "ores_compute_app_version_platforms_tbl" (
    "tenant_id"           uuid not null,
    "app_version_id"      uuid not null,
    "platform_id"         uuid not null,
    "modified_by"         text not null,
    "performed_by"        text not null,
    "change_reason_code"  text not null,
    "change_commentary"   text not null,
    "valid_from"          timestamp with time zone not null,
    "valid_to"            timestamp with time zone not null,
    primary key (tenant_id, app_version_id, platform_id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        app_version_id WITH =,
        platform_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Only one active association per (tenant, app_version, platform) combination
create unique index if not exists ores_compute_app_version_platforms_active_uniq_idx
on "ores_compute_app_version_platforms_tbl" (tenant_id, app_version_id, platform_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_compute_app_version_platforms_tenant_idx
on "ores_compute_app_version_platforms_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_compute_app_version_platforms_app_version_idx
on "ores_compute_app_version_platforms_tbl" (tenant_id, app_version_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_compute_app_version_platforms_insert_fn()
returns trigger as $$
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- If an active record exists for this combination, close it first
    -- (re-enabling a previously removed platform association)
    update "ores_compute_app_version_platforms_tbl"
    set valid_to = current_timestamp
    where tenant_id = NEW.tenant_id
      and app_version_id = NEW.app_version_id
      and platform_id = NEW.platform_id
      and valid_to = ores_utility_infinity_timestamp_fn()
      and valid_from < current_timestamp;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_compute_app_version_platforms_insert_trg
before insert on "ores_compute_app_version_platforms_tbl"
for each row execute function ores_compute_app_version_platforms_insert_fn();

create or replace rule ores_compute_app_version_platforms_delete_rule as
on delete to "ores_compute_app_version_platforms_tbl" do instead
    update "ores_compute_app_version_platforms_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and app_version_id = OLD.app_version_id
      and platform_id = OLD.platform_id
      and valid_to = ores_utility_infinity_timestamp_fn();
