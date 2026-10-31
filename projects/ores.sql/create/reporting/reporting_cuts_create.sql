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
 * Cut Table
 *
 * The record of one cut: a body of data fixed at one timepoint, the four inputs
 * the valuation reads, the gaps the assembly could not fill, and the acts that
 * made it. A report's run resolves its selection to a cut and prices from the
 * body the cut names, so a result is reproduced by naming its cut rather than by
 * re-reading a rule.
 *
 * The state a reader sees is a function of these facts and is stored nowhere, as
 * the published set's state is. Draft when no taking instant holds and no gap
 * stands; incomplete when no taking instant holds and a gap stands; taken when the
 * body is fixed, the kind requires an authorisation and none is recorded; in use
 * when the body is fixed and the kind requires none; authorised when an approval
 * is recorded; refused when a refusal is recorded; superseded when a later cut of
 * the same kind and scope holds its place.
 *
 * The label a person quotes is derived from the kind, the scope and the timepoint,
 * and the take ordinal where a body is taken more than once for one occasion:
 * EOD.ACME-LONDON.2026-10-06 and EOD.ACME-LONDON.2026-10-06.2. A live cut
 * carries its instant to the second instead of an ordinal. No column holds the
 * label.
 *
 * The retention is a copy and not a read: a cut takes the kind's retention_months,
 * retention_destination_code and legal_hold_default when it is taken, so a
 * later change to the kind does not move the retention of a body already fixed.
 * The indicative mark is a second copy for the same reason: a cut records whether a
 * result priced from it is indicative, taken from its kind at the take, because the
 * mark is a fact about the body and not about its state.
 * Nothing removes a cut today; the archive is a later task.
 */

create table if not exists "ores_reporting_cuts_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "kind_code" text not null,
    "party_id" uuid not null,
    "scope_code" text not null,
    "business_centre_code" text not null,
    "timepoint" timestamp with time zone not null,
    "take_ordinal" integer not null,
    "timepoint_timezone" text not null,
    "cut_time_code" text null,
    "market_data_snapshot_id" uuid null,
    "deal_population_id" uuid null,
    "reference_data_version" text null,
    "engine_version" text null,
    "code_version" text null,
    "taken_by" text not null,
    "taken_at" timestamp with time zone null,
    "requires_authorisation" boolean not null default false,
    "approval_request_id" uuid null,
    "approved_by" text not null,
    "approved_at" timestamp with time zone null,
    "refused_at" timestamp with time zone null,
    "refusal_comment" text not null,
    "retention_months" integer not null,
    "retention_destination_code" text not null,
    "legal_hold" boolean not null default false,
    "output_is_indicative" boolean not null default false,
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
    check ("take_ordinal" > 0),
    check ("retention_months" > 0),
    check ("retention_destination_code" in ('cold', 'archive')),
    check ("timepoint_timezone" <> ''),
    check (("approved_at" is null) = ("approved_by" = '')),
    check (("refused_at" is null) = ("refusal_comment" = '')),
    check ("approved_at" is null or "refused_at" is null),
    check ("requires_authorisation" or "approval_request_id" is null)
);

-- Composite natural key: unique combination for active records
create unique index if not exists cuts_occasion_uniq_idx
on "ores_reporting_cuts_tbl" (tenant_id, kind_code, party_id, scope_code, business_centre_code, timepoint, take_ordinal)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists cuts_version_uniq_idx
on "ores_reporting_cuts_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists cuts_id_uniq_idx
on "ores_reporting_cuts_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists cuts_tenant_idx
on "ores_reporting_cuts_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists cuts_kind_timepoint_idx
on "ores_reporting_cuts_tbl" (tenant_id, kind_code, timepoint desc)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists cuts_scope_timepoint_idx
on "ores_reporting_cuts_tbl" (tenant_id, scope_code, timepoint desc)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists cuts_approval_request_idx
on "ores_reporting_cuts_tbl" (tenant_id, approval_request_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_reporting_cuts_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate kind_code (soft FK to ores_reporting_cut_kinds_tbl)
    if not exists (
        select 1 from ores_reporting_cut_kinds_tbl
        where (tenant_id = NEW.tenant_id
           or tenant_id = ores_utility_system_tenant_id_fn())
          and code = NEW.kind_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid kind_code: %. No active cut kind found with this code.', NEW.kind_code
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

    -- Validate business_centre_code (soft FK to ores_refdata_business_centres_tbl)
    if not exists (
        select 1 from ores_refdata_business_centres_tbl
        where tenant_id = NEW.tenant_id
          and code = NEW.business_centre_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid business_centre_code: %. No active business centre found with this code.', NEW.business_centre_code
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
    from "ores_reporting_cuts_tbl"
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
                    'cut',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'cut',
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
        update "ores_reporting_cuts_tbl"
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

create or replace trigger ores_reporting_cuts_insert_trg
before insert on "ores_reporting_cuts_tbl"
for each row execute function ores_reporting_cuts_insert_fn();

create or replace rule ores_reporting_cuts_delete_rule as
on delete to "ores_reporting_cuts_tbl" do instead (
    update "ores_reporting_cuts_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Cut
-- =============================================================================
alter table ores_reporting_cuts_tbl enable row level security;

drop policy if exists cuts_tbl_tenant_isolation_policy
    on ores_reporting_cuts_tbl;

create policy cuts_tbl_tenant_isolation_policy
on ores_reporting_cuts_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);

-- Party isolation (RESTRICTIVE): ANDed with the permissive tenant
-- policy above, a session sees only rows whose party_id its visible
-- party set admits. The visible_party_ids-is-null passthrough applies
-- for sessions with no party restriction (tenant admins, service
-- contexts).
drop policy if exists cuts_tbl_party_isolation_policy
    on ores_reporting_cuts_tbl;

create policy cuts_tbl_party_isolation_policy
on ores_reporting_cuts_tbl
as restrictive
for all using (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
)
with check (
    ores_iam_visible_party_ids_fn() is null
    or party_id = ANY(ores_iam_visible_party_ids_fn())
);
