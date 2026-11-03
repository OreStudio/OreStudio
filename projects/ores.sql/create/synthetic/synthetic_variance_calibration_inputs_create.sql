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
 * Variance Calibration Input Table
 *
 * One capture of the data a variance calibration reads: the item that was read for a
 * pair, under which calibration policy, over which window, and whether its value was
 * inverted to reach the pair's market convention. Each fixing date of the window is
 * a variance_calibration_input_point, kept whether it was used or not, so a reader
 * can see every number a fit saw. A fit reads only a stored input, never the live
 * fixings, so the same input always gives the same fit. See "A calibration is stored
 * and reviewed before a feed runs it" in
 * [[id:351CB816-28F5-428A-A289-414B6F4B9F36][Volatility clustering and GARCH models]].
 *
 * Who captured the input and when are the performed_by and valid_from every row
 * carries. A capture over the same window of the same item is a new version of the
 * input, not a second input, by the natural key: the party, the fixing URI and the
 * window. The item names the pair it is read for, so the pair is not part of the key.
 */

create table if not exists "ores_synthetic_variance_calibration_inputs_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "fixing_uri" text not null,
    "window_end" date not null,
    "window_days" integer not null,
    "base_currency_code" text not null,
    "quote_currency_code" text not null,
    "provider_code" text not null,
    "inverted" boolean not null,
    "policy_id" uuid not null,
    "policy_version" integer not null,
    "point_count" integer not null,
    "used_count" integer not null,
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
    check ("base_currency_code" <> "quote_currency_code"),
    check ("fixing_uri" <> ''),
    check ("provider_code" <> ''),
    check ("window_days" between 30 and 3660),
    check ("point_count" >= 0),
    check ("used_count" between 0 and "point_count")
);

-- Composite natural key: unique combination for active records
create unique index if not exists variance_calibration_inputs_party_id_fixing_uri_window_end_window_days_uniq_idx
on "ores_synthetic_variance_calibration_inputs_tbl" (tenant_id, party_id, fixing_uri, window_end, window_days)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists variance_calibration_inputs_version_uniq_idx
on "ores_synthetic_variance_calibration_inputs_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists variance_calibration_inputs_id_uniq_idx
on "ores_synthetic_variance_calibration_inputs_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists variance_calibration_inputs_tenant_idx
on "ores_synthetic_variance_calibration_inputs_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_synthetic_variance_calibration_inputs_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate base_currency_code
    NEW.base_currency_code := ores_refdata_validate_currency_fn(NEW.tenant_id, NEW.base_currency_code);

    -- Validate quote_currency_code
    NEW.quote_currency_code := ores_refdata_validate_currency_fn(NEW.tenant_id, NEW.quote_currency_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_synthetic_variance_calibration_inputs_tbl"
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
                    'variance_calibration_input',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'variance_calibration_input',
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
        update "ores_synthetic_variance_calibration_inputs_tbl"
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

create or replace trigger ores_synthetic_variance_calibration_inputs_insert_trg
before insert on "ores_synthetic_variance_calibration_inputs_tbl"
for each row execute function ores_synthetic_variance_calibration_inputs_insert_fn();

create or replace rule ores_synthetic_variance_calibration_inputs_delete_rule as
on delete to "ores_synthetic_variance_calibration_inputs_tbl" do instead (
    update "ores_synthetic_variance_calibration_inputs_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      -- A tenant reads the shared rows of the system tenant, so OLD can be one.
      -- The rule runs with the owner's rights, which row-level security does
      -- not bind, so the rule states the own-tenant limit that the delete
      -- policy cannot enforce here.
      and tenant_id = ores_iam_current_tenant_id_fn()
      and valid_to = ores_utility_infinity_timestamp_fn();
);
