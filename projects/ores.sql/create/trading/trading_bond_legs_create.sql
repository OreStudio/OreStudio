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
 * Bond Leg Table
 *
 * One row per leg an instrument states, keyed to the instrument, the list
 * the leg belongs to and the leg's ordinal within that list.
 *
 * The ORE schema declares the bond's leg list unbounded and a bond states
 * one leg per coupon. The total return swap, the repo and the ascot each
 * state at most one leg of their own, and all four are the same leg
 * shape. One table holds them and leg_role says which list the row
 * belongs to.
 *
 * The row carries the leg's payment terms, its day counters, its
 * settlement block and the two flags the schema states on the leg itself.
 * Everything else the leg states lives in a table of its own: the
 * schedules in instrument_schedule, the amortizations, the named
 * amounts and the rate group below. Those tables key on leg_role as
 * well, so a schedule or an amount reaches the leg that stated it.
 *
 * The leg's currency and day counter also reach the issue row, because
 * the issue is where a reader looks for the coupon terms. The row here
 * is the document's own statement and wins on export, so a leg whose
 * terms differ from the issue's still round trips.
 *
 * Every member the schema declares optional is nullable here and an
 * std::optional in C++, so a member the document states and the row
 * cannot hold stays distinguishable from one the document omits.
 */

create table if not exists "ores_trading_bond_legs_tbl" (
    "instrument_id" uuid not null,
    "leg_role" text not null,
    "leg_number" integer not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "payer" boolean null,
    "leg_type" text null,
    "currency" text null,
    "payment_convention" text null,
    "payment_lag" text null,
    "payment_calendar" text null,
    "day_counter" text null,
    "last_period_day_counter" text null,
    "notional_payment_lag" bigint null,
    "strict_notional_dates" boolean null,
    "indexings_from_asset_leg" boolean null,
    "settlement_fx_index" text null,
    "settlement_fixing_date" text null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, instrument_id, leg_role, leg_number, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        instrument_id WITH =,
        leg_role WITH =,
        leg_number WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("instrument_id" <> ores_utility_nil_uuid_fn()),
    check ("leg_role" <> ''),
    check ("leg_role" in ('bond', 'trs_funding', 'repo', 'ascot_swap')),
    check ("leg_number" > 0)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists bond_legs_version_uniq_idx
on "ores_trading_bond_legs_tbl" (tenant_id, instrument_id, leg_role, leg_number, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists bond_legs_id_uniq_idx
on "ores_trading_bond_legs_tbl" (tenant_id, instrument_id, leg_role, leg_number)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists bond_legs_tenant_idx
on "ores_trading_bond_legs_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_bond_legs_insert_fn()
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
    from "ores_trading_bond_legs_tbl"
    where tenant_id = NEW.tenant_id
      and instrument_id = NEW.instrument_id and leg_role = NEW.leg_role and leg_number = NEW.leg_number
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
        update "ores_trading_bond_legs_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and instrument_id = NEW.instrument_id and leg_role = NEW.leg_role and leg_number = NEW.leg_number
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

create or replace trigger ores_trading_bond_legs_insert_trg
before insert on "ores_trading_bond_legs_tbl"
for each row execute function ores_trading_bond_legs_insert_fn();

create or replace rule ores_trading_bond_legs_delete_rule as
on delete to "ores_trading_bond_legs_tbl" do instead (
    update "ores_trading_bond_legs_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and instrument_id = OLD.instrument_id and leg_role = OLD.leg_role and leg_number = OLD.leg_number
      and valid_to = ores_utility_infinity_timestamp_fn();
);
