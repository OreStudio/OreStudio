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
 * Approval Decision Table
 *
 * A request is decided by many decisions, not one: a kind may need two approvers,
 * and a hold, a return to waiting, a withdrawal, a refusal and a reversal are each
 * a decision of their own. Each is a row here, and a decision is never edited.
 *
 * Two rules of the record are rules of the schema. The person who asked cannot
 * decide their own request, and a withdrawal is the one decision only they take:
 * a hand-written trigger in inbox_approval_decision_rules_create.sql checks
 * both. One person approves a request at most once: the unique index below
 * states it.
 */

create table if not exists "ores_inbox_approval_decisions_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "request_id" uuid not null,
    "decision_code" text not null,
    "decided_by" uuid not null,
    "decided_at" timestamp with time zone not null,
    "comment" text not null,
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

-- Version uniqueness for optimistic concurrency
create unique index if not exists approval_decisions_version_uniq_idx
on "ores_inbox_approval_decisions_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists approval_decisions_id_uniq_idx
on "ores_inbox_approval_decisions_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists approval_decisions_tenant_idx
on "ores_inbox_approval_decisions_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists approval_decisions_one_approval_per_person_idx
on "ores_inbox_approval_decisions_tbl" (tenant_id, request_id, decided_by)
where valid_to = ores_utility_infinity_timestamp_fn()
  and decision_code = 'approve';

create or replace function ores_inbox_approval_decisions_insert_fn()
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

    -- Validate decision_code (soft FK to ores_inbox_approval_decision_types_tbl)
    if not exists (
        select 1 from ores_inbox_approval_decision_types_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.decision_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid decision_code: %. No approval decision type found with this code.', NEW.decision_code
            using errcode = '23503';
    end if;

    -- Validate decided_by (soft FK to ores_iam_accounts_tbl)
    if not exists (
        select 1 from ores_iam_accounts_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.decided_by
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid decided_by: %. No account found with this id.', NEW.decided_by
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
    from "ores_inbox_approval_decisions_tbl"
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
                    'approval_decision',
                    'id',
                    NEW.id::text);
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'approval_decision',
                'id',
                NEW.id::text,
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_inbox_approval_decisions_tbl"
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

create or replace trigger ores_inbox_approval_decisions_insert_trg
before insert on "ores_inbox_approval_decisions_tbl"
for each row execute function ores_inbox_approval_decisions_insert_fn();

create or replace rule ores_inbox_approval_decisions_delete_rule as
on delete to "ores_inbox_approval_decisions_tbl" do instead (
    update "ores_inbox_approval_decisions_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Approval Decision
-- =============================================================================
alter table ores_inbox_approval_decisions_tbl enable row level security;

drop policy if exists approval_decisions_tbl_tenant_isolation_policy
    on ores_inbox_approval_decisions_tbl;

create policy approval_decisions_tbl_tenant_isolation_policy
on ores_inbox_approval_decisions_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
