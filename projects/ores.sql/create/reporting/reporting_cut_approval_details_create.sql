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
 * Cut Approval Detail Table
 *
 * The detail row of one reporting.eod_cut_authorisation or
 * reporting.ipv_cut_authorisation approval request: the cut the request
 * authorises, and the counts of the body the decider is asked to sign.
 *
 * A request is a header. What it is about lives here, keyed by the request's own
 * id, exactly as a proposed published set lives in
 * ores.marketdata.curve_publish and is keyed by its request. The subject is one
 * fact and no more: the cut whose body was frozen. The counts travel beside it
 * because the decider reads them before answering and the cut's own record states
 * them only by reading the body's parts one at a time: the deals the population
 * held at the instant, the published sets in use at it, and the gaps that would
 * have stopped the take, which is zero for any body a request was raised for.
 *
 * applied_at and applied_by are the record of the apply. Both are empty while
 * the request waits, and both are written in one act when the approved request is
 * applied to the cut: the instant the authorisation was stamped and the account
 * that stamped it. The row is bitemporal, so the applied version is a new version
 * of the same row and the history holds the unapplied subject beside it.
 * request_id is the natural key, so a request authorises one cut and no second
 * apply can find a row to write.
 *
 * The row is not a client's to write: the take writes it with the body, and the
 * apply stamps it. A client reads it to decide a request or to audit a cut.
 */

create table if not exists "ores_reporting_cut_approval_details_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "request_id" uuid not null,
    "party_id" uuid not null,
    "cut_id" uuid not null,
    "deal_count" integer not null,
    "market_data_set_count" integer not null,
    "gap_count" integer not null,
    "applied_by" text not null,
    "applied_at" timestamp with time zone null,
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
    check ("id" <> ores_utility_nil_uuid_fn()),
    check (("applied_at" is null) = ("applied_by" = '')),
    check ("deal_count" >= 0),
    check ("market_data_set_count" >= 0),
    check ("gap_count" >= 0)
);

-- Unique request_id for active records
create unique index if not exists cut_approval_details_request_id_uniq_idx
on "ores_reporting_cut_approval_details_tbl" (tenant_id, request_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists cut_approval_details_version_uniq_idx
on "ores_reporting_cut_approval_details_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists cut_approval_details_id_uniq_idx
on "ores_reporting_cut_approval_details_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists cut_approval_details_tenant_idx
on "ores_reporting_cut_approval_details_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists cut_approval_details_cut_approval_details_cut_idx
on "ores_reporting_cut_approval_details_tbl" (tenant_id, cut_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_cut_approval_details_insert_fn()
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

    -- Validate party_id (soft FK to ores_refdata_parties_tbl)
    if not exists (
        select 1 from ores_refdata_parties_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid party_id: %. No active party found with this id.', NEW.party_id
            using errcode = '23503';
    end if;

    -- Validate cut_id (soft FK to ores_reporting_cuts_tbl)
    if not exists (
        select 1 from ores_reporting_cuts_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.cut_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid cut_id: %. No active cut found with this id.', NEW.cut_id
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
    from "ores_reporting_cut_approval_details_tbl"
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
                    'cut_approval_detail',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'cut_approval_detail',
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
        update "ores_reporting_cut_approval_details_tbl"
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

create or replace trigger ores_reporting_cut_approval_details_insert_trg
before insert on "ores_reporting_cut_approval_details_tbl"
for each row execute function ores_reporting_cut_approval_details_insert_fn();

create or replace rule ores_reporting_cut_approval_details_delete_rule as
on delete to "ores_reporting_cut_approval_details_tbl" do instead (
    update "ores_reporting_cut_approval_details_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Cut Approval Detail
-- =============================================================================
alter table ores_reporting_cut_approval_details_tbl enable row level security;

drop policy if exists cut_approval_details_tbl_tenant_isolation_policy
    on ores_reporting_cut_approval_details_tbl;

create policy cut_approval_details_tbl_tenant_isolation_policy
on ores_reporting_cut_approval_details_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
