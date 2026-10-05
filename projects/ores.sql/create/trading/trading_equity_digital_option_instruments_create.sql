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
 * Equity Digital Option Instrument Table
 *
 * Represents EquityDigitalOption and EquityTouchOption trades.
 * underlying_name captures the ORE equity Name identifier;
 * option_type is Call or Put (digital only); strike is digital
 * only; barrier_level and barrier_type are touch only; the two
 * product families are mutually exclusive, enforced by a cross-column
 * check. expiry_date is an ISO 8601 date string.
 */

create table if not exists "ores_trading_equity_digital_option_instruments_tbl" (
    "trade_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "trade_type_code" text not null,
    "party_id" uuid not null,
    "underlying_name" text not null,
    "currency" text not null,
    "notional" numeric(28, 10) not null,
    "option_type" text null,
    "strike" numeric(28, 10) null,
    "barrier_level" numeric(28, 10) null,
    "barrier_type" text null,
    "expiry_date" date not null,
    "long_short" text not null,
    "payout_amount" numeric(28, 10) null,
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
    check ("trade_type_code" in ('EquityTouchOption', 'EquityDigitalOption')),
    check ("notional" > 0),
    check ("underlying_name" <> ''),
    check ("currency" <> ''),
    check (("trade_type_code" = 'EquityDigitalOption' and "option_type" is not null and "strike" is not null and "barrier_level" is null and "barrier_type" is null) or ("trade_type_code" = 'EquityTouchOption' and "barrier_level" is not null and "barrier_type" is not null and "option_type" is null and "strike" is null)),
    check ("option_type" is null or "option_type" in ('Call', 'Put')),
    check ("barrier_type" is null or "barrier_type" in ('UpAndOut', 'UpAndIn', 'DownAndOut', 'DownAndIn', 'KnockIn', 'KnockOut', 'CumulatedProfitCap', 'CumulatedProfitCapPoints', 'FixingCap', 'FixingFloor'))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists equity_digital_option_instruments_version_uniq_idx
on "ores_trading_equity_digital_option_instruments_tbl" (tenant_id, trade_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists equity_digital_option_instruments_id_uniq_idx
on "ores_trading_equity_digital_option_instruments_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists equity_digital_option_instruments_tenant_idx
on "ores_trading_equity_digital_option_instruments_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists equity_digital_option_instruments_party_idx
on "ores_trading_equity_digital_option_instruments_tbl" (tenant_id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_equity_digital_option_instruments_insert_fn()
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
    ) then
        raise exception 'Invalid trade_id: %. Trade must exist for tenant.', NEW.trade_id
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
    from "ores_trading_equity_digital_option_instruments_tbl"
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
        update "ores_trading_equity_digital_option_instruments_tbl"
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

create or replace trigger ores_trading_equity_digital_option_instruments_insert_trg
before insert on "ores_trading_equity_digital_option_instruments_tbl"
for each row execute function ores_trading_equity_digital_option_instruments_insert_fn();

create or replace rule ores_trading_equity_digital_option_instruments_delete_rule as
on delete to "ores_trading_equity_digital_option_instruments_tbl" do instead (
    update "ores_trading_equity_digital_option_instruments_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
