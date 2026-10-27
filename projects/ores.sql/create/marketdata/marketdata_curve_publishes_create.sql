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
 * Curve Publish Table
 *
 * The detail row of one marketdata.curve_publish approval request: the subject
 * the request authorises, and the record of the act once the subject is published.
 *
 * A request is a header. What it is about lives here, keyed by the request's own
 * id, exactly as a proposed book change lives in ores.refdata.book_change and is
 * keyed by its request. The subject is three facts and no more: the curve
 * configuration whose output is published, the as-of instant the bootstrap reads
 * the market at, and the party the configuration belongs to (party_id). The curve
 * is named by its configuration because that is what the publish call names. The
 * series the output is written to (output_series_id) is carried beside it because
 * the reader that serves a published curve starts from a series and not from a
 * configuration.
 *
 * published_at and published_by are the publication record. Both are empty
 * while the request waits, and both are written in one act when the approved
 * subject is published: the timestamp is the instant the published set moved, and
 * the username is the account that applied it. The row is bitemporal, so the
 * applied version is a new version of the same row and the history holds the
 * unapplied subject beside it. request_id is the natural key, so a request
 * authorises one publish and no second apply can find a row to write.
 *
 * The row is not a client's to write: the raise writes it, the gate reads it, and
 * the apply stamps it. A client reads it to decide a request or to audit a curve.
 */

create table if not exists "ores_marketdata_curve_publishes_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "request_id" uuid not null,
    "party_id" uuid not null,
    "bootstrap_config_id" uuid not null,
    "output_series_id" uuid not null,
    "as_of" timestamp with time zone not null,
    "published_by" text not null,
    "published_at" timestamp with time zone null,
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
    check (("published_at" is null) = ("published_by" = '')),
    check ("as_of" > '1970-01-01 00:00:00+00'::timestamp with time zone)
);

-- Unique request_id for active records
create unique index if not exists curve_publishes_request_id_uniq_idx
on "ores_marketdata_curve_publishes_tbl" (tenant_id, request_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists curve_publishes_version_uniq_idx
on "ores_marketdata_curve_publishes_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists curve_publishes_id_uniq_idx
on "ores_marketdata_curve_publishes_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists curve_publishes_tenant_idx
on "ores_marketdata_curve_publishes_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists curve_publishes_curve_publishes_series_published_idx
on "ores_marketdata_curve_publishes_tbl" (tenant_id, output_series_id, published_at desc)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_marketdata_curve_publishes_insert_fn()
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

    -- Validate bootstrap_config_id (soft FK to ores_refdata_ir_curve_bootstrap_configs_tbl)
    if not exists (
        select 1 from ores_refdata_ir_curve_bootstrap_configs_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.bootstrap_config_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid bootstrap_config_id: %. No active curve bootstrap configuration found.', NEW.bootstrap_config_id
            using errcode = '23503';
    end if;

    -- Validate output_series_id (soft FK to ores_marketdata_market_series_tbl)
    if not exists (
        select 1 from ores_marketdata_market_series_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.output_series_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid output_series_id: %. No active market series found.', NEW.output_series_id
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
    from "ores_marketdata_curve_publishes_tbl"
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
                    'curve_publish',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'curve_publish',
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
        update "ores_marketdata_curve_publishes_tbl"
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

create or replace trigger ores_marketdata_curve_publishes_insert_trg
before insert on "ores_marketdata_curve_publishes_tbl"
for each row execute function ores_marketdata_curve_publishes_insert_fn();

create or replace rule ores_marketdata_curve_publishes_delete_rule as
on delete to "ores_marketdata_curve_publishes_tbl" do instead (
    update "ores_marketdata_curve_publishes_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Curve Publish
-- =============================================================================
alter table ores_marketdata_curve_publishes_tbl enable row level security;

drop policy if exists curve_publishes_tbl_tenant_isolation_policy
    on ores_marketdata_curve_publishes_tbl;

create policy curve_publishes_tbl_tenant_isolation_policy
on ores_marketdata_curve_publishes_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
