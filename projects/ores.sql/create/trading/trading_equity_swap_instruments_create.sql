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
 * Equity Swap Instrument Table
 *
 * Routes ORE product types: EquitySwap, EquityWorstOfBasketSwap. For
 * basket swaps, underlying_name is NULL and basket_json holds the basket
 * definition; for single-name swaps, basket_json is NULL.
 */

create table if not exists "ores_trading_equity_swap_instruments_tbl" (
    "trade_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "trade_type_code" text not null,
    "party_id" uuid not null,
    "trade_activity_id" uuid not null,
    "underlying_name" text null,
    "basket_json" text null,
    "currency" text not null,
    "notional" numeric(38, 12) not null,
    "return_type" text not null,
    "start_date" date not null,
    "maturity_date" date not null,
    "long_short" text not null,
    "payment_frequency_code" text not null,
    "description" text null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, trade_id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        trade_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("trade_id" <> ores_utility_nil_uuid_fn()),
    check ("trade_type_code" in ('EquitySwap', 'EquityWorstOfBasketSwap')),
    check (("trade_type_code" = 'EquityWorstOfBasketSwap' and "basket_json" is not null and "underlying_name" is null) or ("trade_type_code" = 'EquitySwap' and "underlying_name" is not null and "basket_json" is null)),
    check ("notional" > 0),
    check ("currency" <> ''),
    check ("return_type" in ('Total', 'Price'))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists equity_swap_instruments_version_uniq_idx
on "ores_trading_equity_swap_instruments_tbl" (tenant_id, trade_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists equity_swap_instruments_id_uniq_idx
on "ores_trading_equity_swap_instruments_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists equity_swap_instruments_tenant_idx
on "ores_trading_equity_swap_instruments_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists equity_swap_instruments_party_idx
on "ores_trading_equity_swap_instruments_tbl" (tenant_id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists equity_swap_instruments_trade_type_idx
on "ores_trading_equity_swap_instruments_tbl" (tenant_id, trade_type_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_equity_swap_instruments_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Set party_id from session context
    NEW.party_id := current_setting('app.current_party_id')::uuid;

    -- Validate trade_id (soft FK to ores_trading_trades_tbl)
    if not exists (
        select 1 from ores_trading_trades_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.trade_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid trade_id: %. Trade must exist for tenant.', NEW.trade_id
            using errcode = '23503';
    end if;

    -- Validate trade_activity_id (soft FK to ores_trading_trade_activities_tbl)
    if not exists (
        select 1 from ores_trading_trade_activities_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.trade_activity_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid trade_activity_id: %. No active trade activity found with this id.', NEW.trade_activity_id
            using errcode = '23503';
    end if;

    -- Validate payment_frequency_code
    NEW.payment_frequency_code := ores_refdata_validate_payment_frequency_fn(NEW.tenant_id, NEW.payment_frequency_code);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_trading_equity_swap_instruments_tbl"
    where tenant_id = NEW.tenant_id
      and trade_id = NEW.trade_id
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
                    'equity_swap_instrument',
                    'trade_id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'equity_swap_instrument',
                'trade_id',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_trading_equity_swap_instruments_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and trade_id = NEW.trade_id
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

create or replace trigger ores_trading_equity_swap_instruments_insert_trg
before insert on "ores_trading_equity_swap_instruments_tbl"
for each row execute function ores_trading_equity_swap_instruments_insert_fn();

create or replace rule ores_trading_equity_swap_instruments_delete_rule as
on delete to "ores_trading_equity_swap_instruments_tbl" do instead (
    update "ores_trading_equity_swap_instruments_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
