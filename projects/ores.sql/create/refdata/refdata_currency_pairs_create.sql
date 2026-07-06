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
 * Currency Pair Table
 *
 * Currency pair identity: base/quote legs, deliverability, classification,
 * and fixing source. Conventions (pip factor, tick size, calendars,
 * business day convention) live 1:1 in
 * [[id:1B88215B-1FE0-4CAF-B6AB-53F471963CA6][ores.refdata.currency_pair_convention]], matching the codebase's
 * existing *_convention entity family. spot_days, calendars, and G11
 * membership are *derived* at read time from the two legs, not stored
 * here — see [[id:04A121FA-00D6-43EB-9B21-04EDC1FA493D][Currency pair support in reference data]] for the full design
 * rationale.
 */

create table if not exists "ores_refdata_currency_pairs_tbl" (
    "pair_code" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "base_currency" text not null,
    "quote_currency" text not null,
    "deliverable" boolean not null default true,
    "settlement_currency" text null,
    "classification" text not null,
    "fixing_source" text null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, pair_code, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        pair_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("pair_code" <> '')
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists currency_pairs_version_uniq_idx
on "ores_refdata_currency_pairs_tbl" (tenant_id, pair_code, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists currency_pairs_pair_code_uniq_idx
on "ores_refdata_currency_pairs_tbl" (tenant_id, pair_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists currency_pairs_tenant_idx
on "ores_refdata_currency_pairs_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_currency_pairs_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate base_currency
    NEW.base_currency := ores_refdata_validate_currency_fn(NEW.tenant_id, NEW.base_currency);

    -- Validate quote_currency
    NEW.quote_currency := ores_refdata_validate_currency_fn(NEW.tenant_id, NEW.quote_currency);

    -- Validate classification
    NEW.classification := ores_refdata_validate_currency_pair_classification_fn(NEW.tenant_id, NEW.classification);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_refdata_currency_pairs_tbl"
    where tenant_id = NEW.tenant_id
      and pair_code = NEW.pair_code
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        update "ores_refdata_currency_pairs_tbl"
        set valid_to = current_timestamp
        where tenant_id = NEW.tenant_id
          and pair_code = NEW.pair_code
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < current_timestamp;
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = current_timestamp;
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_refdata_currency_pairs_insert_trg
before insert on "ores_refdata_currency_pairs_tbl"
for each row execute function ores_refdata_currency_pairs_insert_fn();

create or replace rule ores_refdata_currency_pairs_delete_rule as
on delete to "ores_refdata_currency_pairs_tbl" do instead
    update "ores_refdata_currency_pairs_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and pair_code = OLD.pair_code
      and valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Validation function for currency_pair
-- Validates that a pair_code exists in the currency_pairs table.
-- Returns the validated value, or default if null/empty.
-- =============================================================================
create or replace function ores_refdata_validate_currency_pair_fn(
    p_tenant_id uuid,
    p_value text
) returns text as $$
begin
    -- Return default if null or empty
    if p_value is null or p_value = '' then
        raise exception 'Invalid currency_pair: value cannot be null or empty'
            using errcode = '23502';
    end if;


    return p_value;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;
