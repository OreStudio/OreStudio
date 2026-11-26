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
 * Retained Store Table
 *
 * Every store the installation keeps is a row here, before anything else knows
 * about it. The row is what lets a person read "Calendar dates" instead of
 * ores_refdata_calendar_dates_tbl, and what lets the sweep, the screen and the
 * alerts agree on the set of stores without any of them carrying a list of tables.
 *
 * The catalogue is not a policy: it states what the store is and the bound it ships
 * with, and a policy states what is applied now. That is why a store exists here
 * whether or not a policy names it, and why the bound is one of three kinds rather
 * than always an action and a period. A store bounded by its content keying needs
 * no policy, and a store whose bound cannot be expressed as an age — the calendar
 * days behind the horizon — states that it has none rather than carrying a default
 * that would delete what pricing still needs.
 *
 * The rows describe the installation's own stores, so the platform authors them and
 * the system tenant holds them: the classification that governs published rows
 * makes the platform the author here, as it does for the policies those rows
 * describe. The column is not null for that reason.
 */

create table if not exists "ores_scheduler_retained_stores_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "display_name" text not null,
    "description" text not null,
    "owner_component" text not null,
    "scope" text not null,
    "target_kind" text not null,
    "target" text not null,
    "time_column" text null,
    "measure_kind" text not null,
    "bound_kind" text not null,
    "default_action" text null,
    "default_retention_days" integer null,
    "default_cadence_hours" integer not null,
    "default_reason" text null,
    "modified_by" text not null,
    "performed_by" text not null,
    "acting_agent" text not null default '',
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
    check ("code" <> ''),
    check ("scope" in ('platform', 'tenant')),
    check ("target_kind" in ('chunked_table', 'table', 'object_prefix', 'directory')),
    check ("measure_kind" in ('relation_rows', 'objects', 'files')),
    check ("bound_kind" in ('policy', 'keying', 'none')),
    check ("default_action" is null or "default_action" in ('drop_chunks', 'delete_rows', 'expire_prefix', 'release_directory')),
    check (case when "bound_kind" = 'policy' then "default_action" is not null and coalesce("default_retention_days" > 0, false) else "default_action" is null and "default_retention_days" is null end),
    check ("default_cadence_hours" > 0),
    check ("target" <> ''),
    check ("time_column" is not null or "target_kind" in ('object_prefix', 'directory'))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists retained_stores_version_uniq_idx
on "ores_scheduler_retained_stores_tbl" (tenant_id, code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists retained_stores_code_uniq_idx
on "ores_scheduler_retained_stores_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists retained_stores_tenant_idx
on "ores_scheduler_retained_stores_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_scheduler_retained_stores_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_scheduler_retained_stores_tbl"
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
                    'retained_store',
                    'code');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'retained_store',
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
        update "ores_scheduler_retained_stores_tbl"
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
    NEW.acting_agent = coalesce(ores_iam_current_agent_fn(), '');

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_scheduler_retained_stores_insert_trg
before insert on "ores_scheduler_retained_stores_tbl"
for each row execute function ores_scheduler_retained_stores_insert_fn();

create or replace rule ores_scheduler_retained_stores_delete_rule as
on delete to "ores_scheduler_retained_stores_tbl" do instead (
    update "ores_scheduler_retained_stores_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and code = OLD.code
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Retained Store
-- Every session reads its own rows and the system tenant's rows; writes
-- stay the current tenant's.
-- =============================================================================
alter table ores_scheduler_retained_stores_tbl enable row level security;

drop policy if exists retained_stores_tbl_tenant_isolation_policy
    on ores_scheduler_retained_stores_tbl;

create policy retained_stores_tbl_tenant_isolation_policy
on ores_scheduler_retained_stores_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- The read admits the system tenant's rows beside the caller's; the policy
-- above still binds every write to the caller's own rows. It must: a single
-- for-all policy that admitted them would let a tenant change or remove a
-- shared row. See doc/knowledge/security/data_read_across_tenants.org.
drop policy if exists retained_stores_tbl_shared_reference_read_policy
    on ores_scheduler_retained_stores_tbl;

create policy retained_stores_tbl_shared_reference_read_policy
on ores_scheduler_retained_stores_tbl
for select using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
);
