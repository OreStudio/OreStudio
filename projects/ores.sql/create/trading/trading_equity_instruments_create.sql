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

-- =============================================================================
-- Equity Instruments Table
--
-- Holds equity economics for all equity ORE product types. The trade_type_code
-- discriminates the exact product. Optional fields are NULL when not applicable
-- to the sub-type:
--   option_type/strike_price/exercise_type: option products only
--   barrier_type/lower_barrier/upper_barrier: barrier option products
--   average_type/averaging_start_date: Asian/average-price options
--   variance_strike: variance swap products
--   cliquet_frequency_code: cliquet options
--   accumulation_amount/knock_out_barrier: accumulator/TaRF products
--   basket_json: basket/rainbow/worst-of products
--   day_count_code/payment_frequency_code/return_type: swap products
--   quantity: position products
-- =============================================================================

create table if not exists "ores_trading_equity_instruments_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "trade_id" uuid null,
    "trade_type_code" text not null,
    "underlying_code" text not null,
    "currency" text not null,
    "notional" numeric(28, 10) not null,
    "quantity" numeric(28, 10) null,
    "start_date" date null,
    "maturity_date" date null,
    "option_type" text null,
    "strike_price" numeric(28, 10) null,
    "exercise_type" text null,
    "barrier_type" text null,
    "lower_barrier" numeric(28, 10) null,
    "upper_barrier" numeric(28, 10) null,
    "average_type" text null,
    "averaging_start_date" date null,
    "variance_strike" numeric(28, 10) null,
    "cliquet_frequency_code" text null,
    "accumulation_amount" numeric(28, 10) null,
    "knock_out_barrier" numeric(28, 10) null,
    "basket_json" text null,
    "day_count_code" text null,
    "payment_frequency_code" text null,
    "return_type" text null,
    "description" text null,
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
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid),
    check ("notional" > 0),
    check ("underlying_code" <> ''),
    check ("currency" <> ''),
    check ("option_type" is null or "option_type" in ('Call', 'Put')),
    check ("exercise_type" is null or "exercise_type" in ('European', 'American', 'Bermudan')),
    check ("average_type" is null or "average_type" in ('Arithmetic', 'Geometric')),
    check ("barrier_type" is null or "barrier_type" in ('UpIn', 'UpOut', 'DownIn', 'DownOut')),
    check ("return_type" is null or "return_type" in ('TotalReturn', 'PriceReturn'))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists ores_trading_equity_instruments_version_uniq_idx
on "ores_trading_equity_instruments_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Current record uniqueness
create unique index if not exists ores_trading_equity_instruments_id_uniq_idx
on "ores_trading_equity_instruments_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Tenant index
create index if not exists ores_trading_equity_instruments_tenant_idx
on "ores_trading_equity_instruments_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Party index for party isolation
create index if not exists ores_trading_equity_instruments_party_idx
on "ores_trading_equity_instruments_tbl" (tenant_id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Trade type index for product filtering
create index if not exists ores_trading_equity_instruments_trade_type_idx
on "ores_trading_equity_instruments_tbl" (tenant_id, trade_type_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Soft FK back to trade (NULL for standalone instruments)
create unique index if not exists ores_trading_equity_instruments_trade_id_idx
on "ores_trading_equity_instruments_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn()
  and trade_id is not null;

create or replace function ores_trading_equity_instruments_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Set party_id from session context
    NEW.party_id := current_setting('app.current_party_id')::uuid;

    -- Validate trade_type_code
    NEW.trade_type_code := ores_trading_validate_trade_type_fn(NEW.tenant_id, NEW.trade_type_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_trading_equity_instruments_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if NEW.version != 0 and NEW.version != current_version then
            raise exception 'Version conflict: expected version %, but current version is %',
                NEW.version, current_version
                using errcode = 'P0002';
        end if;
        NEW.version = current_version + 1;

        update "ores_trading_equity_instruments_tbl"
        set valid_to = current_timestamp
        where tenant_id = NEW.tenant_id
          and id = NEW.id
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
$$ language plpgsql security definer;

create or replace trigger ores_trading_equity_instruments_insert_trg
before insert on "ores_trading_equity_instruments_tbl"
for each row execute function ores_trading_equity_instruments_insert_fn();

create or replace rule ores_trading_equity_instruments_delete_rule as
on delete to "ores_trading_equity_instruments_tbl" do instead
    update "ores_trading_equity_instruments_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
