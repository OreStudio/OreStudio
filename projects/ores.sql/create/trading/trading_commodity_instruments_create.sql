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
 * Commodity Instrument Table
 *
 * Represents every commodity product type ORE states. trade_type_code
 * discriminates the exact product, and each optional field block is null
 * when the sub-type does not state it: the option block for option
 * products, the pricing block for average-price and spread products, and
 * the exotic block for variance, accumulator, barrier and basket
 * products.
 *
 * A basket product's constituents are not a column: each is a row of
 * ores.trading.commodity_basket_constituent, keyed to this instrument and
 * its ordinal in the document. A text column held the list as a JSON array
 * and could not be typed, indexed or questioned, so the collection is a
 * child table now.
 */

create table if not exists "ores_trading_commodity_instruments_tbl" (
    "instrument_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "trade_type_code" text not null,
    "party_id" uuid not null,
    "trade_id" uuid null,
    "commodity_code" text not null,
    "currency" text not null,
    "quantity" numeric(28, 10) not null,
    "unit" text not null,
    "start_date" date null,
    "maturity_date" date null,
    "fixed_price" numeric(28, 10) null,
    "option_type" text null,
    "strike_price" numeric(28, 10) null,
    "exercise_type" text null,
    "average_type" text null,
    "averaging_start_date" date null,
    "averaging_end_date" date null,
    "spread_commodity_code" text null,
    "spread_amount" numeric(28, 10) null,
    "strip_frequency_code" text null,
    "variance_strike" numeric(28, 10) null,
    "accumulation_amount" numeric(28, 10) null,
    "knock_out_barrier" numeric(28, 10) null,
    "barrier_type" text null,
    "lower_barrier" numeric(28, 10) null,
    "upper_barrier" numeric(28, 10) null,
    "day_count_fraction_code" text null,
    "payment_frequency_code" text null,
    "swaption_expiry_date" date null,
    "description" text null,
    "workspace_id" uuid not null default ores_utility_live_workspace_id_fn(), -- soft FK to ores_workspaces_tbl(id)
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
    check ("quantity" > 0),
    check ("commodity_code" <> ''),
    check ("currency" <> ''),
    check ("unit" <> ''),
    check ("option_type" is null or "option_type" in ('Call', 'Put')),
    check ("exercise_type" is null or "exercise_type" in ('European', 'American')),
    check ("average_type" is null or "average_type" in ('Arithmetic', 'Geometric')),
    check ("barrier_type" is null or "barrier_type" in ('UpAndIn', 'UpAndOut', 'DownAndIn', 'DownAndOut')),
    check ("trade_type_code" in ('CompositeTrade', 'RateDigitalOption', 'SwaptionStraddle', 'ForwardVolatilityAgreement', 'Swap', 'CrossCurrencySwap', 'ForwardRateAgreement', 'CapFloor', 'Swaption', 'FlexiSwap', 'BalanceGuaranteedSwap', 'CallableSwap', 'KnockOutSwap', 'RiskParticipationAgreement', 'InflationSwap', 'FxForwardVolatilityAgreement', 'FxForward', 'FxSwap', 'FxOption', 'FxDigitalOption', 'FxAverageForward', 'FxAsianOption', 'FxBarrierOption', 'FxDoubleBarrierOption', 'FxEuropeanBarrierOption', 'FxWindowBarrierOption', 'FxGenericBarrierOption', 'FxKIKOBarrierOption', 'FxTouchOption', 'FxDoubleTouchOption', 'FxDigitalBarrierOption', 'FxVarianceSwap', 'FxPairwiseVarianceSwap', 'FxBasketVarianceSwap', 'FxAccumulator', 'FxTaRF', 'FxWorstOfBasketSwap', 'FxBestEntryOption', 'FxBasketOption', 'FxRainbowOption', 'FxStrikeResettableOption', 'CreditDefaultSwap', 'CreditDefaultSwapOption', 'IndexCreditDefaultSwap', 'IndexCreditDefaultSwapOption', 'SyntheticCDO', 'CreditLinkedSwap', 'CBO', 'BondFutureOption', 'Bond', 'ForwardBond', 'BondFuture', 'BondOption', 'BondRepo', 'BondTRS', 'BondPosition', 'CallableBond', 'ConvertibleBond', 'Ascot', 'EquityAutoDeltaHedgedOption', 'EquityForwardVolatilityAgreement', 'EquityOption', 'EquityFutureOption', 'EquityAsianOption', 'EquityBarrierOption', 'EquityDoubleBarrierOption', 'EquityEuropeanBarrierOption', 'EquityWindowBarrierOption', 'EquityGenericBarrierOption', 'EquityTouchOption', 'EquityDoubleTouchOption', 'EquityDigitalOption', 'EquityForward', 'EquitySwap', 'EquityVarianceSwap', 'EquityPairwiseVarianceSwap', 'EquityBasketVarianceSwap', 'EquityCliquetOption', 'EquityAccumulator', 'EquityTaRF', 'EquityWorstOfBasketSwap', 'EquityBestEntryOption', 'EquityBasketOption', 'EquityRainbowOption', 'EquityOutperformanceOption', 'EquityStrikeResettableOption', 'TotalReturnSwap', 'ContractForDifference', 'EquityPosition', 'EquityOptionPosition', 'CommodityForwardVolatilityAgreement', 'IntradayPowerForward', 'CommodityForward', 'CommodityOption', 'CommodityDigitalOption', 'CommodityDigitalAveragePriceOption', 'CommodityAsianOption', 'CommodityAveragePriceOption', 'CommoditySpreadOption', 'CommodityOptionStrip', 'CommoditySwap', 'CommoditySwaption', 'CommodityVarianceSwap', 'CommodityPairwiseVarianceSwap', 'CommodityBasketVarianceSwap', 'CommodityAccumulator', 'CommodityTaRF', 'CommodityWorstOfBasketSwap', 'CommodityBestEntryOption', 'CommodityWindowBarrierOption', 'CommodityGenericBarrierOption', 'CommodityBasketOption', 'CommodityRainbowOption', 'CommodityStrikeResettableOption', 'CommodityPosition', 'CashPosition', 'ScriptedTrade', 'Autocallable_01', 'DoubleDigitalOption', 'EuropeanOptionBarrier', 'PerformanceOption_01'))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists commodity_instruments_version_uniq_idx
on "ores_trading_commodity_instruments_tbl" (tenant_id, instrument_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists commodity_instruments_id_uniq_idx
on "ores_trading_commodity_instruments_tbl" (tenant_id, instrument_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists commodity_instruments_tenant_idx
on "ores_trading_commodity_instruments_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists commodity_instruments_party_idx
on "ores_trading_commodity_instruments_tbl" (tenant_id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists commodity_instruments_trade_id_idx
on "ores_trading_commodity_instruments_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn()
  and trade_id is not null;

create index if not exists commodity_instruments_trade_type_idx
on "ores_trading_commodity_instruments_tbl" (tenant_id, trade_type_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists commodity_instruments_workspace_idx
on "ores_trading_commodity_instruments_tbl" (workspace_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_commodity_instruments_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate workspace_id
    NEW.workspace_id := ores_workspace_validate_fn(NEW.workspace_id);

    -- Set party_id from session context
    NEW.party_id := current_setting('app.current_party_id')::uuid;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_trading_commodity_instruments_tbl"
    where tenant_id = NEW.tenant_id
      and instrument_id = NEW.instrument_id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        -- The write states what it believes about the row, and the store is
        -- what decides. Version zero means one thing: no current row exists.
        -- So a create that collides with a live row is refused here, for every
        -- client, rather than by a check each client has to remember.
        if NEW.version = 0 then
            if not ores_utility_version_replace_allowed_fn() then
                raise exception
                    'Row already exists: a create cannot replace it. State the version you read to replace the row, or ask for a version replace.'
                    using errcode = '23505';
            end if;
        elsif NEW.version != current_version then
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
        update "ores_trading_commodity_instruments_tbl"
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

create or replace trigger ores_trading_commodity_instruments_insert_trg
before insert on "ores_trading_commodity_instruments_tbl"
for each row execute function ores_trading_commodity_instruments_insert_fn();

create or replace rule ores_trading_commodity_instruments_delete_rule as
on delete to "ores_trading_commodity_instruments_tbl" do instead (
    update "ores_trading_commodity_instruments_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and instrument_id = OLD.instrument_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
