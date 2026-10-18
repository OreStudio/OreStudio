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
 * Equity Outperformance Option Instrument Table
 *
 * One row per equity outperformance option, keyed by the trade.
 *
 * The option is on the relative performance of two equities, so the pair is the
 * subject matter and both names have a column. That is why this product cannot
 * live in equity_option_instruments, whose single underlying_name states one.
 *
 * The two initial prices are the levels the payoff measures the return from, and
 * the strike return is the threshold the relative return must beat. Both are
 * amounts and rates that settle cash, so both are exact decimals. The return in
 * particular is a proportion that multiplies money, and this family holds a rate
 * as an exact decimal for that reason.
 *
 * The knock prices are levels the document may omit, so both are nullable.
 *
 * The table is named equity_outperformance_options rather than
 * equity_outperformance_option_instruments. The derived name is one character
 * over the limit the generator allows once the trigger and policy suffixes are
 * counted, and PostgreSQL would truncate it mid-word.
 */

create table if not exists "ores_trading_equity_outperformance_options_tbl" (
    "trade_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "trade_activity_id" uuid not null,
    "underlying1_name" text not null,
    "underlying2_name" text not null,
    "currency" text not null,
    "notional" numeric(38, 12) not null,
    "long_short" text not null,
    "initial_price1" numeric(38, 12) not null,
    "initial_price2" numeric(38, 12) not null,
    "strike_return" numeric(38, 12) not null,
    "knock_in_price" numeric(38, 12) null,
    "knock_out_price" numeric(38, 12) null,
    "initial_price_currency1" text null,
    "initial_price_currency2" text null,
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
    check ("underlying1_name" <> ''),
    check ("underlying2_name" <> ''),
    check ("long_short" in ('Long', 'Short')),
    check ("strike_return" >= 0),
    check ("underlying1_name" <> "underlying2_name")
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists equity_outperformance_option_instruments_version_uniq_idx
on "ores_trading_equity_outperformance_options_tbl" (tenant_id, trade_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists equity_outperformance_option_instruments_id_uniq_idx
on "ores_trading_equity_outperformance_options_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists equity_outperformance_option_instruments_tenant_idx
on "ores_trading_equity_outperformance_options_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_equity_outperformance_options_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_trading_equity_outperformance_options_tbl"
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
                    'equity_outperformance_option_instrument',
                    'trade_id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'equity_outperformance_option_instrument',
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
        update "ores_trading_equity_outperformance_options_tbl"
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

create or replace trigger ores_trading_equity_outperformance_options_insert_trg
before insert on "ores_trading_equity_outperformance_options_tbl"
for each row execute function ores_trading_equity_outperformance_options_insert_fn();

create or replace rule ores_trading_equity_outperformance_options_delete_rule as
on delete to "ores_trading_equity_outperformance_options_tbl" do instead (
    update "ores_trading_equity_outperformance_options_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Equity Outperformance Option Instrument
-- =============================================================================
alter table ores_trading_equity_outperformance_options_tbl enable row level security;

drop policy if exists equity_outperformance_options_tbl_tenant_isolation_policy
    on ores_trading_equity_outperformance_options_tbl;

create policy equity_outperformance_options_tbl_tenant_isolation_policy
on ores_trading_equity_outperformance_options_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
