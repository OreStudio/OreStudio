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
 * Engine Settings Change Table
 *
 * A number-changing group of engine run settings never changes in place. A write
 * to such a group is a proposal: the values the person asked for are held here,
 * one row for each setting of the group, and the version in force keeps the rows a
 * run reads until the request is signed off.
 *
 * The apply reads the rows of an approved request and writes the group it names
 * with the values stated here, so a proposal that waits changes no part.
 *
 * A row names the setting by its code, which is the group and the column joined,
 * so a proposal survives a change to the columns of a part.
 */

create table if not exists "ores_reporting_engine_settings_changes_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "request_id" uuid not null,
    "engine_settings_id" uuid not null,
    "group_code" text not null,
    "setting_code" text not null,
    "text_value" text null,
    "base_version" integer not null,
    "applied" boolean not null,
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

-- Composite natural key: unique combination for active records
create unique index if not exists engine_settings_changes_request_id_engine_settings_id_group_code_setting_code_uniq_idx
on "ores_reporting_engine_settings_changes_tbl" (tenant_id, request_id, engine_settings_id, group_code, setting_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists engine_settings_changes_version_uniq_idx
on "ores_reporting_engine_settings_changes_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists engine_settings_changes_id_uniq_idx
on "ores_reporting_engine_settings_changes_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists engine_settings_changes_tenant_idx
on "ores_reporting_engine_settings_changes_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_engine_settings_changes_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate request_id (soft FK to ores_inbox_approval_requests_tbl)
    if not exists (
        select 1 from ores_inbox_approval_requests_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.request_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid request_id: %. No approval request found with this id.', NEW.request_id
            using errcode = '23503';
    end if;

    -- Validate engine_settings_id (soft FK to ores_reporting_engine_settings_tbl)
    if not exists (
        select 1 from ores_reporting_engine_settings_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.engine_settings_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid engine_settings_id: %. No active engine settings found with this id.', NEW.engine_settings_id
            using errcode = '23503';
    end if;

    -- Validate group_code (soft FK to ores_reporting_engine_settings_groups_tbl)
    if not exists (
        select 1 from ores_reporting_engine_settings_groups_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.group_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid group_code: %. No engine settings group found with this code.', NEW.group_code
            using errcode = '23503';
    end if;

    -- Validate setting_code (soft FK to ores_reporting_engine_settings_group_settings_tbl)
    if not exists (
        select 1 from ores_reporting_engine_settings_group_settings_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.setting_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid setting_code: %. No engine settings group setting found with this code.', NEW.setting_code
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_reporting_engine_settings_changes_tbl"
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
                perform ores_outcome_raise_fn(
                    'already_exists',
                    'engine_settings_change',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'engine_settings_change',
                'id',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_reporting_engine_settings_changes_tbl"
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
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_reporting_engine_settings_changes_insert_trg
before insert on "ores_reporting_engine_settings_changes_tbl"
for each row execute function ores_reporting_engine_settings_changes_insert_fn();

create or replace rule ores_reporting_engine_settings_changes_delete_rule as
on delete to "ores_reporting_engine_settings_changes_tbl" do instead (
    update "ores_reporting_engine_settings_changes_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Engine Settings Change
-- =============================================================================
alter table ores_reporting_engine_settings_changes_tbl enable row level security;

drop policy if exists engine_settings_changes_tbl_tenant_isolation_policy
    on ores_reporting_engine_settings_changes_tbl;

create policy engine_settings_changes_tbl_tenant_isolation_policy
on ores_reporting_engine_settings_changes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
