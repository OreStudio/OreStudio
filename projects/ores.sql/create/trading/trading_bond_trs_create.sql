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
 * Bond TRS Table
 *
 * One row per bond total return swap trade, keyed by the instrument row
 * it extends. The columns fix the ER row ("return type, funding index
 * or rate") from the return side (totalReturnData, instruments.xsd
 * lines 2313-2336) and the funding leg (fundingData lines 2297-2301
 * wrapping one legData).
 *
 * Three members of the return side ride here because no other row holds
 * them: the payer flag, the price type and the initial price. The return
 * schedule lands as schedule rows in the shared instrument-keyed
 * schedule tables, under the owner role trs, and the funding leg's own
 * terms land in the shared leg family.
 *
 * The return side states nine more members that no table holds:
 * ObservationLag, ObservationConvention, ObservationCalendar,
 * PaymentLag, PaymentConvention, PaymentCalendar, PaymentDates,
 * FXConversion and FXTerms. The corpus states none of them, so the
 * round trip is whole without them, and they are a recorded scope limit.
 */

create table if not exists "ores_trading_bond_trs_tbl" (
    "instrument_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "return_type" text not null,
    "funding_leg_type" text not null,
    "funding_rate" numeric(28, 10) null,
    "funding_index" text null,
    "payer" text null,
    "price_type" text null,
    "initial_price" numeric(28, 10) null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, instrument_id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        instrument_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("instrument_id" <> ores_utility_nil_uuid_fn()),
    check ("return_type" in ('TotalReturn', 'PriceReturn')),
    check ("funding_leg_type" in ('Fixed', 'Floating'))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists bond_trs_version_uniq_idx
on "ores_trading_bond_trs_tbl" (tenant_id, instrument_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists bond_trs_id_uniq_idx
on "ores_trading_bond_trs_tbl" (tenant_id, instrument_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists bond_trs_tenant_idx
on "ores_trading_bond_trs_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_bond_trs_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_trading_bond_trs_tbl"
    where tenant_id = NEW.tenant_id
      and instrument_id = NEW.instrument_id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_trading_bond_trs_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and instrument_id = NEW.instrument_id
          and valid_to = ores_utility_infinity_timestamp_fn()
          and valid_from < clock_timestamp();
    else
        NEW.version = 1;
    end if;

    NEW.valid_from = clock_timestamp();
    NEW.valid_to = ores_utility_infinity_timestamp_fn();
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_trading_bond_trs_insert_trg
before insert on "ores_trading_bond_trs_tbl"
for each row execute function ores_trading_bond_trs_insert_fn();

create or replace rule ores_trading_bond_trs_delete_rule as
on delete to "ores_trading_bond_trs_tbl" do instead (
    update "ores_trading_bond_trs_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and instrument_id = OLD.instrument_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
