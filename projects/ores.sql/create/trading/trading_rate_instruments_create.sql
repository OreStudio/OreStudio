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
 * Rate Instrument Table
 *
 * One row per interest-rates trade, the family's own header. It holds what
 * every product in the family states the same way: the trade it belongs to,
 * the trade type code, the party, the activity that wrote the version, the
 * instrument's start and maturity dates and its description. It holds no
 * product economics.
 *
 * The header exists for two reasons. It is the family's *coverage*: the
 * trade_type_code check that the routed-instrument template generates from
 * ores.trading.trade_type_catalogue lists exactly the codes routed here, so
 * the family's codes sit in one table and no product table repeats the
 * identity columns. And it is the family's *owner*: the * Delete cascade
 * section below names every table the family writes, so the delete rule closes
 * the legs, leg children and facts with the header instead of leaving them
 * behind (finding R6 of the family's design).
 *
 * The products' own fields stay in their fact tables, keyed by the same trade
 * and joined to this row by the trade id, exactly as bond_instruments joins
 * its facts. A fact table carries no party_id and no trade_type_code: the
 * header is the family's party boundary and its routing boundary, so a fact
 * row reaches its tenant and its code through the header (design decision D3).
 *
 * The dates are the family's *common dates*. Each product used to state them
 * under its own name — start_date, maturity_date, expiry_date,
 * end_date — so nothing could read one date column across the family
 * (finding R9). The two columns here are the one set of names; a product that
 * means something else by a date keeps that date in its own fact table.
 *
 * InflationSwap is a member until an inflation family is scaffolded. The
 * taxonomy already calls it an inflation product, and its columns move here
 * with the rest of the family's so that its rows are owned and routed like
 * every other rates row. It leaves with its own family's rework.
 */

create table if not exists "ores_trading_rate_instruments_tbl" (
    "trade_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "trade_type_code" text not null,
    "party_id" uuid not null,
    "trade_activity_id" uuid not null,
    "start_date" date null,
    "maturity_date" date null,
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
    check ("trade_type_code" in ('Swap', 'CrossCurrencySwap', 'ForwardRateAgreement', 'CapFloor', 'Swaption', 'FlexiSwap', 'BalanceGuaranteedSwap', 'CallableSwap', 'KnockOutSwap', 'InflationSwap')),
    check ("maturity_date" is null or "start_date" is null or "maturity_date" > "start_date")
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists rate_instruments_version_uniq_idx
on "ores_trading_rate_instruments_tbl" (tenant_id, trade_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists rate_instruments_id_uniq_idx
on "ores_trading_rate_instruments_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists rate_instruments_tenant_idx
on "ores_trading_rate_instruments_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists rate_instruments_party_idx
on "ores_trading_rate_instruments_tbl" (tenant_id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists rate_instruments_trade_type_idx
on "ores_trading_rate_instruments_tbl" (tenant_id, trade_type_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_rate_instruments_insert_fn()
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

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_trading_rate_instruments_tbl"
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
                    'rate_instrument',
                    'trade_id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'rate_instrument',
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
        update "ores_trading_rate_instruments_tbl"
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

create or replace trigger ores_trading_rate_instruments_insert_trg
before insert on "ores_trading_rate_instruments_tbl"
for each row execute function ores_trading_rate_instruments_insert_fn();

-- The rows this row owns, closed when it is deleted. A function and not a
-- rule body, because the rule resolves the tables it names when it is
-- created and a child may be created after its parent; a plpgsql body
-- resolves them when it runs.
create or replace function ores_trading_rate_instruments_cascade_delete_fn(
    p_row "ores_trading_rate_instruments_tbl")
returns void as $$
begin
    -- Close every row this row owns, so one delete removes the family and
    -- not the header alone. The store enforces it, so every caller gets it.
    delete from "ores_trading_instrument_strikes_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_instrument_schedules_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_instrument_schedule_dates_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_instrument_options_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_instrument_option_premiums_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_instrument_option_exercise_fees_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_instrument_option_payment_dates_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_swap_legs_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_swap_leg_amounts_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_swap_leg_rates_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_callable_swap_call_dates_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_vanilla_swap_instruments_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_fra_instruments_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_cap_floor_instruments_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_swaption_instruments_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_balance_guaranteed_swap_instruments_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_callable_swap_instruments_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_knock_out_swap_instruments_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_inflation_swap_instruments_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace rule ores_trading_rate_instruments_delete_rule as
on delete to "ores_trading_rate_instruments_tbl" do instead (
    select ores_trading_rate_instruments_cascade_delete_fn(OLD);
    update "ores_trading_rate_instruments_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
