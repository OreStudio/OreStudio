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
 * Notification Argument Table
 *
 * The values a notification's message names, one row each: the role, the tenant,
 * the step. The screen renders the kind's message with them in the reader's
 * language, so a notification stores no finished sentence and no document.
 *
 * A row is identified by the notification and the argument's name.
 */

create table if not exists "ores_inbox_notification_arguments_tbl" (
    "notification_id" uuid not null,
    "tenant_id" uuid not null,
    "name" text not null,
    "version" integer not null,
    "value" text not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, notification_id, name, valid_from),
    exclude using gist (
        tenant_id WITH =,
        notification_id WITH =,
        name WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Index for reading the arguments of a notification
create index if not exists notification_arguments_notification_idx
on "ores_inbox_notification_arguments_tbl" (notification_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Index for finding notifications by argument name
create index if not exists notification_arguments_name_idx
on "ores_inbox_notification_arguments_tbl" (name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique constraint on active records for ON CONFLICT support
create unique index if not exists notification_arguments_uniq_idx
on "ores_inbox_notification_arguments_tbl" (tenant_id, notification_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists notification_arguments_tenant_idx
on "ores_inbox_notification_arguments_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_inbox_notification_arguments_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- The actor is validated before the version management below: the
    -- validator accepts a username only while a current account row holds
    -- it, and a self write retires that row.
    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);

    -- Version management
    select version into current_version
    from "ores_inbox_notification_arguments_tbl"
    where tenant_id = new.tenant_id
    and notification_id = new.notification_id
    and name = new.name
    and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'notification_arguments',
                'notification_id',
                new.notification_id::text,
                new.version::text,
                current_version::text);
        end if;
        new.version = current_version + 1;

        -- Close existing record.
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. the same junction pair touched
        -- twice in one transaction) would collide with itself.
        -- clock_timestamp() always advances.
        update "ores_inbox_notification_arguments_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = new.tenant_id
        and notification_id = new.notification_id
        and name = new.name
        and valid_to = ores_utility_infinity_timestamp_fn()
        and valid_from < clock_timestamp();
    else
        new.version = 1;
    end if;

    new.valid_from = clock_timestamp();
    new.valid_to = ores_utility_infinity_timestamp_fn();

    new.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    -- Validate notification_id (soft FK to ores_inbox_notifications_tbl)
    if not exists (
        select 1 from ores_inbox_notifications_tbl
        where tenant_id = new.tenant_id
          and id = new.notification_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid notification_id: %. No notification found with this id.', new.notification_id
            using errcode = '23503';
    end if;

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_inbox_notification_arguments_insert_trg
before insert on "ores_inbox_notification_arguments_tbl"
for each row
execute function ores_inbox_notification_arguments_insert_fn();

create or replace rule ores_inbox_notification_arguments_delete_rule as
on delete to "ores_inbox_notification_arguments_tbl"
do instead
  update "ores_inbox_notification_arguments_tbl"
  set valid_to = clock_timestamp()
  where tenant_id = old.tenant_id
  and notification_id = old.notification_id
  and name = old.name
  and valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Row-level security: tenant isolation for Notification Argument
-- =============================================================================
alter table "ores_inbox_notification_arguments_tbl" enable row level security;

drop policy if exists notification_arguments_tbl_tenant_isolation_policy
    on "ores_inbox_notification_arguments_tbl";

create policy notification_arguments_tbl_tenant_isolation_policy
on "ores_inbox_notification_arguments_tbl"
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
