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
 * Instrument Option Table
 *
 * One row per instrument whose document stated an option block, keyed to
 * the instrument.
 *
 * The bond option and the ascot both state the same optionData element,
 * and the equity, FX and commodity products state it too. The nine bond
 * tables carry the option's type and its strike and nothing else, so this
 * table holds the block: the exercise and payment terms, the settlement
 * terms, and the three nested groups whose lists get child tables of
 * their own.
 *
 * A member the document omits is a null column. Three members are
 * themselves groups whose every member is optional, so a null column set
 * cannot say whether the document stated the group and left it bare or
 * omitted it. Each of those three carries a flag on this row:
 * has_exercise_data, has_payment_data and has_settlement_data.
 *
 * Every other member is text, and the text is the document's own
 * spelling. The schema types the premium amount, the exercise price list
 * and the several flags as text or as its own bool, which enumerates
 * thirteen spellings, so a decoded form would not re-emit what the
 * document held.
 */

create table if not exists "ores_trading_instrument_options_tbl" (
    "instrument_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "long_short" text not null,
    "option_type" text null,
    "payoff_type" text null,
    "payoff_type_2" text null,
    "style" text null,
    "notice_period" text null,
    "notice_calendar" text null,
    "notice_convention" text null,
    "mid_coupon_exercise" text null,
    "settlement" text null,
    "settlement_method" text null,
    "pay_off_at_expiry" text null,
    "premium_amount" text null,
    "premium_currency" text null,
    "premium_pay_date" text null,
    "exercise_prices" text null,
    "exercise_fee_settlement_period" text null,
    "exercise_fee_settlement_calendar" text null,
    "exercise_fee_settlement_convention" text null,
    "automatic_exercise" text null,
    "has_exercise_data" boolean not null,
    "exercise_date" date null,
    "exercise_price" numeric(28, 10) null,
    "has_payment_data" boolean not null,
    "payment_lag" integer null,
    "payment_calendar" text null,
    "payment_convention" text null,
    "payment_relative_to" text null,
    "has_settlement_data" boolean not null,
    "settlement_pay_currency" text null,
    "settlement_fx_index" text null,
    "settlement_fixing_date" text null,
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
    check ("long_short" in ('Long', 'Short'))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists instrument_options_version_uniq_idx
on "ores_trading_instrument_options_tbl" (tenant_id, instrument_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists instrument_options_id_uniq_idx
on "ores_trading_instrument_options_tbl" (tenant_id, instrument_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists instrument_options_tenant_idx
on "ores_trading_instrument_options_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_instrument_options_insert_fn()
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
    from "ores_trading_instrument_options_tbl"
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
        update "ores_trading_instrument_options_tbl"
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

create or replace trigger ores_trading_instrument_options_insert_trg
before insert on "ores_trading_instrument_options_tbl"
for each row execute function ores_trading_instrument_options_insert_fn();

create or replace rule ores_trading_instrument_options_delete_rule as
on delete to "ores_trading_instrument_options_tbl" do instead (
    update "ores_trading_instrument_options_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and instrument_id = OLD.instrument_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
