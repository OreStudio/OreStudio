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
 * Book Change Table
 *
 * Do not edit. Change ores.refdata.book and run
 * derive_pending_models.py.
 */

create table if not exists "ores_refdata_book_changes_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "request_id" uuid not null,
    "line_no" integer not null,
    "operation" text not null,
    "base_version" integer not null,
    "entity_id" uuid not null,
    "party_id" uuid not null,
    "name" text not null,
    "description" text null,
    "parent_portfolio_id" uuid not null,
    "owner_unit_id" uuid null,
    "functional_currency" text not null,
    "gl_account_ref" text null,
    "cost_center" text null,
    "book_status" text not null,
    "regulatory_book_type" text not null,
    "book_purpose_type" text not null,
    "ledger_feed_type" text not null,
    "is_sweepable" boolean not null,
    "rates_centre_code" text not null,
    "sandbox_id" uuid null,
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
create unique index if not exists book_changes_request_id_line_no_uniq_idx
on "ores_refdata_book_changes_tbl" (tenant_id, request_id, line_no)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists book_changes_version_uniq_idx
on "ores_refdata_book_changes_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists book_changes_id_uniq_idx
on "ores_refdata_book_changes_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists book_changes_tenant_idx
on "ores_refdata_book_changes_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_book_changes_insert_fn()
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

    -- Validate parent_portfolio_id (soft FK to ores_refdata_portfolios_tbl)
    if not exists (
        select 1 from ores_refdata_portfolios_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.parent_portfolio_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid parent_portfolio_id: %. No active portfolio found with this id.', NEW.parent_portfolio_id
            using errcode = '23503';
    end if;

    -- Validate owner_unit_id (optional soft FK to ores_refdata_business_units_tbl)
    if NEW.owner_unit_id is not null then
        if not exists (
            select 1 from ores_refdata_business_units_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.owner_unit_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid owner_unit_id: %. No active business unit found.', NEW.owner_unit_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate sandbox_id (optional soft FK to ores_refdata_sandboxes_tbl)
    if NEW.sandbox_id is not null then
        if not exists (
            select 1 from ores_refdata_sandboxes_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.sandbox_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid sandbox_id: %. No active sandbox found with this id.', NEW.sandbox_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate functional_currency
    NEW.functional_currency := ores_refdata_validate_currency_fn(NEW.tenant_id, NEW.functional_currency);

    -- Validate book_status
    NEW.book_status := ores_refdata_validate_book_status_fn(NEW.tenant_id, NEW.book_status);

    -- Validate regulatory_book_type
    NEW.regulatory_book_type := ores_refdata_validate_regulatory_book_type_fn(NEW.tenant_id, NEW.regulatory_book_type);

    -- Validate book_purpose_type
    NEW.book_purpose_type := ores_refdata_validate_book_purpose_type_fn(NEW.tenant_id, NEW.book_purpose_type);

    -- Validate ledger_feed_type
    NEW.ledger_feed_type := ores_refdata_validate_ledger_feed_type_fn(NEW.tenant_id, NEW.ledger_feed_type);

    -- Validate rates_centre_code
    NEW.rates_centre_code := ores_refdata_validate_business_centre_fn(NEW.tenant_id, NEW.rates_centre_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_refdata_book_changes_tbl"
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
                    'book_change',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'book_change',
                'id',
                NEW.version::text,
                current_version::text);
        end if;
        if exists (
            select 1 from "ores_refdata_book_changes_tbl"
            where tenant_id = NEW.tenant_id
              and id = NEW.id
              and valid_to = ores_utility_infinity_timestamp_fn()
              and "sandbox_id" is distinct from NEW."sandbox_id"
        ) then
            raise exception 'sandbox_id cannot change: it is fixed for the life of the book_change.'
                using errcode = '23514';
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_refdata_book_changes_tbl"
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

create or replace trigger ores_refdata_book_changes_insert_trg
before insert on "ores_refdata_book_changes_tbl"
for each row execute function ores_refdata_book_changes_insert_fn();

create or replace rule ores_refdata_book_changes_delete_rule as
on delete to "ores_refdata_book_changes_tbl" do instead (
    update "ores_refdata_book_changes_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
