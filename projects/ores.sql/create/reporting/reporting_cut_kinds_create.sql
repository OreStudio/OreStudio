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
 * Cut Kind Table
 *
 * +#+updated: 2026-10-31
 *
 * Reference data table of the kinds of cut. A kind is not a sentence in a
 * knowledge page: it is a row a person maintains, because four of the facts it
 * carries are decisions a firm changes and reads back.
 *
 *
 * approval_kind_code is a nullable foreign key to the inbox's own lookup. It is
 * the whole of the authorisation rule: the end of day family's kinds point at
 * reporting.eod_cut_authorisation, whose decider is Finance, the verification
 * kind points at reporting.ipv_cut_authorisation, whose decider is Market Risk,
 * and the start of day, intraday and consumer kinds point at nothing. A screen
 * draws the words from the key's target and a dash where the key is null, so a
 * firm that gives the close to another function changes one approval kind and no
 * cut kind.
 *
 * Nothing a screen prints about authorisation is stored: the signed-or-not a
 * person reads is derived from this column.
 *
 * retention_months states the periods the design fixes: 25 for the ordinary
 * close and the open, 84 for the month end and the verification, 120 for the year
 * end, 3 for the intraday view and 1 for the live view. retention_destination_code
 * names where a cut goes when it leaves the warm tier. No storage tier lookup
 * exists yet, so the column carries a check on the codes this round knows and the
 * archive is a later task.
 */

create table if not exists "ores_reporting_cut_kinds_tbl" (
    "code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "description" text not null,
    "family_code" text not null,
    "approval_kind_code" text null,
    "output_is_indicative" boolean not null default false,
    "resolution_rule_code" text not null,
    "retention_months" integer not null,
    "retention_destination_code" text not null,
    "legal_hold_default" boolean not null default false,
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
    check ("code" <> ''),
    check ("retention_months" > 0),
    check ("retention_destination_code" in ('cold', 'archive'))
);

-- Unique name for active records
create unique index if not exists cut_kinds_name_uniq_idx
on "ores_reporting_cut_kinds_tbl" (tenant_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists cut_kinds_version_uniq_idx
on "ores_reporting_cut_kinds_tbl" (tenant_id, code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists cut_kinds_code_uniq_idx
on "ores_reporting_cut_kinds_tbl" (tenant_id, code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists cut_kinds_tenant_idx
on "ores_reporting_cut_kinds_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_cut_kinds_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate family_code (soft FK to ores_reporting_cut_families_tbl)
    if not exists (
        select 1 from ores_reporting_cut_families_tbl
        where (tenant_id = NEW.tenant_id
           or tenant_id = ores_utility_system_tenant_id_fn())
          and code = NEW.family_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid family_code: %. No active cut family found with this code.', NEW.family_code
            using errcode = '23503';
    end if;

    -- Validate approval_kind_code (optional soft FK to ores_inbox_approval_kinds_tbl)
    if NEW.approval_kind_code is not null then
        if not exists (
            select 1 from ores_inbox_approval_kinds_tbl
            where (tenant_id = NEW.tenant_id
               or tenant_id = ores_utility_system_tenant_id_fn())
              and code = NEW.approval_kind_code
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid approval_kind_code: %. No active approval kind found with this code.', NEW.approval_kind_code
                using errcode = '23503';
        end if;
    end if;

    -- Validate resolution_rule_code (soft FK to ores_reporting_cut_resolution_rules_tbl)
    if not exists (
        select 1 from ores_reporting_cut_resolution_rules_tbl
        where (tenant_id = NEW.tenant_id
           or tenant_id = ores_utility_system_tenant_id_fn())
          and code = NEW.resolution_rule_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid resolution_rule_code: %. No active resolution rule found with this code.', NEW.resolution_rule_code
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
    from "ores_reporting_cut_kinds_tbl"
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
                    'cut_kind',
                    'code');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'cut_kind',
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
        update "ores_reporting_cut_kinds_tbl"
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

create or replace trigger ores_reporting_cut_kinds_insert_trg
before insert on "ores_reporting_cut_kinds_tbl"
for each row execute function ores_reporting_cut_kinds_insert_fn();

create or replace rule ores_reporting_cut_kinds_delete_rule as
on delete to "ores_reporting_cut_kinds_tbl" do instead (
    update "ores_reporting_cut_kinds_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and code = OLD.code
      -- A tenant reads the shared rows of the system tenant, so OLD can be one.
      -- The rule runs with the owner's rights, which row-level security does
      -- not bind, so the rule states the own-tenant limit that the delete
      -- policy cannot enforce here.
      and tenant_id = ores_iam_current_tenant_id_fn()
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Cut Kind
-- Every session reads its own rows and the system tenant's rows; writes
-- stay the current tenant's.
-- =============================================================================
alter table ores_reporting_cut_kinds_tbl enable row level security;

drop policy if exists cut_kinds_tbl_tenant_isolation_policy
    on ores_reporting_cut_kinds_tbl;

create policy cut_kinds_tbl_tenant_isolation_policy
on ores_reporting_cut_kinds_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
    or tenant_id = ores_utility_system_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
