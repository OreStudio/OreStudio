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
 * Approval Policy Table
 *
 * Reference data table of the approval policy: for an act on an entity, optionally
 * narrowed to one field, the part that must approve it. A row says that putting or
 * deleting an entity_type, or changing one of its fields, needs the named part. An
 * act with no row is written directly.
 *
 * The policy is data, so who decides is a data change. The rows are the tables in
 * the book controls note: the description of a book needs no part, the cost center
 * needs finance, a move needs operations. One act may have several rows, one for each
 * part that must approve, and the request's parts are the distinct parts of its rows.
 *
 * The system tenant holds the policy rows, so a tenant cannot loosen its own
 * controls. A tenant's own rows, which may add a part to an act and never remove one,
 * come with the policy read in a later task.
 */

create table if not exists "ores_inbox_approval_policies_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "description" text not null,
    "entity_type" text not null,
    "operation" text not null,
    "field_name" text null,
    "part_code" text not null,
    "display_order" integer not null default 0,
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
create unique index if not exists approval_policies_name_uniq_idx
on "ores_inbox_approval_policies_tbl" (tenant_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists approval_policies_version_uniq_idx
on "ores_inbox_approval_policies_tbl" (tenant_id, code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists approval_policies_code_uniq_idx
on "ores_inbox_approval_policies_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists approval_policies_tenant_idx
on "ores_inbox_approval_policies_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_inbox_approval_policies_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate part_code (soft FK to ores_inbox_approval_parts_tbl)
    if not exists (
        select 1 from ores_inbox_approval_parts_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.part_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid part_code: %. No approval part found with this code.', NEW.part_code
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
    from "ores_inbox_approval_policies_tbl"
    where tenant_id = NEW.tenant_id
      and code = NEW.code
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
                    'approval_policy',
                    'code');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'approval_policy',
                'code',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_inbox_approval_policies_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and code = NEW.code
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

create or replace trigger ores_inbox_approval_policies_insert_trg
before insert on "ores_inbox_approval_policies_tbl"
for each row execute function ores_inbox_approval_policies_insert_fn();

create or replace rule ores_inbox_approval_policies_delete_rule as
on delete to "ores_inbox_approval_policies_tbl" do instead (
    update "ores_inbox_approval_policies_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and code = OLD.code
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Validation function for approval_policy
-- Validates that a code exists in the approval_policies table.
-- Returns the validated value, or default if null/empty.
-- Uses system tenant data (shared reference data).
-- =============================================================================
create or replace function ores_inbox_validate_approval_policy_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
begin
    -- Return default if null or empty
    if p_value is null or p_value = '' then
        perform ores_outcome_raise_fn('missing_field', 'approval_policy');
    end if;

    -- Allow pass-through during bootstrap (no active rows for system tenant).
    if not exists (
        select 1 from ores_inbox_approval_policies_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        return p_value;
    end if;

    -- Validate against reference data
    if not exists (
        select 1 from ores_inbox_approval_policies_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = p_value
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid approval_policy: %. Must be one of: %', p_value, (
            select string_agg(code::text, ', ' order by display_order)
            from ores_inbox_approval_policies_tbl
            where tenant_id = ores_utility_system_tenant_id_fn()
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) using errcode = '23503';
    end if;

    return p_value;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
