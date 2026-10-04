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
 * Bond Instrument Table
 *
 * One row per bond trade, reshaped from the wide legacy table per the
 * bond relational model deliverable
 * (doc/knowledge/architecture/trading_bond_relational_model.org). The
 * row carries the instrument's identity only: trade_id, the trade that
 * identifies both the trade and its instrument, trade_type_code over the
 * ten bond codes, party_id, and issue_id, the NOT NULL foreign key to the
 * bond issue row that holds every term of the bond. The economics
 * moved to the issue (one row per ISIN, shared by every instrument of
 * it), so an amendment to a term touches the issue row once, not every
 * open instrument (finding B11 of the deliverable). A product change is
 * a cancel and a rebook: the service closes the row's validity and
 * inserts a new instrument row with its fact rows; trade_type_code
 * never changes in place (review answer 2).
 *
 * The ten codes, in seed order (trading_trade_types_populate.sql):
 * Bond, ForwardBond, BondFuture, BondOption, BondRepo, BondTRS,
 * BondPosition, CallableBond, ConvertibleBond, Ascot. The trade_type_code
 * check is the in-list coverage check over the ten codes below, which is
 * the schema's statement of the closed set; the real foreign key to
 * ores_trading_trade_types_tbl is PR 4's (defect 7). For Bond, ForwardBond, CallableBond,
 * ConvertibleBond and BondPosition the issue is the bond itself; for
 * BondRepo the issue is the collateral the financing runs against; for
 * BondOption, BondFuture, BondTRS and Ascot the issue is the bond the
 * product is written on (review answers 3 and 4).
 */

create table if not exists "ores_trading_bond_instruments_tbl" (
    "trade_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "trade_type_code" text not null,
    "party_id" uuid not null,
    "issue_id" uuid not null,
    "notional" numeric(28, 10) null,
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
    check ("trade_type_code" in ('Bond', 'ForwardBond', 'BondFuture', 'BondOption', 'BondRepo', 'BondTRS', 'BondPosition', 'CallableBond', 'ConvertibleBond', 'Ascot'))
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists bond_instruments_version_uniq_idx
on "ores_trading_bond_instruments_tbl" (tenant_id, trade_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists bond_instruments_id_uniq_idx
on "ores_trading_bond_instruments_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists bond_instruments_tenant_idx
on "ores_trading_bond_instruments_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists bond_instruments_party_idx
on "ores_trading_bond_instruments_tbl" (tenant_id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists bond_instruments_trade_type_idx
on "ores_trading_bond_instruments_tbl" (tenant_id, trade_type_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_bond_instruments_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Set party_id from session context
    NEW.party_id := current_setting('app.current_party_id')::uuid;

    -- Validate trade_id (soft FK to ores_trading_trade_anchors_tbl)
    if not exists (
        select 1 from ores_trading_trade_anchors_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.trade_id
    ) then
        raise exception 'Invalid trade_id: %. Trade must exist for tenant.', NEW.trade_id
            using errcode = '23503';
    end if;

    -- Validate issue_id (soft FK to ores_trading_bond_issues_tbl)
    if not exists (
        select 1 from ores_trading_bond_issues_tbl
        where tenant_id = NEW.tenant_id
          and issue_id = NEW.issue_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid issue_id: %. The bond issue must exist for tenant.', NEW.issue_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_trading_bond_instruments_tbl"
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
        update "ores_trading_bond_instruments_tbl"
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
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);
    NEW.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_trading_bond_instruments_insert_trg
before insert on "ores_trading_bond_instruments_tbl"
for each row execute function ores_trading_bond_instruments_insert_fn();

-- The rows this row owns, closed when it is deleted. A function and not a
-- rule body, because the rule resolves the tables it names when it is
-- created and a child may be created after its parent; a plpgsql body
-- resolves them when it runs.
create or replace function ores_trading_bond_instruments_cascade_delete_fn(
    p_row "ores_trading_bond_instruments_tbl")
returns void as $$
begin
    -- Close every row this row owns, so one delete removes the family and
    -- not the header alone. The store enforces it, so every caller gets it.
    delete from "ores_trading_bond_legs_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_bond_leg_amounts_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_bond_leg_rates_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_bond_leg_amortizations_tbl"
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
    delete from "ores_trading_instrument_strikes_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_bond_forwards_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_bond_futures_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_bond_options_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_bond_repos_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_bond_trs_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    delete from "ores_trading_ascots_tbl"
    where tenant_id = p_row.tenant_id
      and trade_id = p_row.trade_id;
    -- A row this one was the last to name goes with it, so a shared parent
    -- survives and its last child does not leave an orphan. This row is
    -- still open here, so the count excludes it. The parent is taken first:
    -- two deletes that share it would otherwise each see the other's row
    -- still open and leave the parent behind.
    perform 1 from "ores_trading_bond_issues_tbl"
    where tenant_id = p_row.tenant_id
      and issue_id = p_row.issue_id
    for update;
    if not exists (
        select 1 from "ores_trading_bond_instruments_tbl"
        where tenant_id = p_row.tenant_id
          and issue_id = p_row.issue_id
          and trade_id <> p_row.trade_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        delete from "ores_trading_bond_issues_tbl"
        where tenant_id = p_row.tenant_id
          and issue_id = p_row.issue_id;
    end if;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace rule ores_trading_bond_instruments_delete_rule as
on delete to "ores_trading_bond_instruments_tbl" do instead (
    select ores_trading_bond_instruments_cascade_delete_fn(OLD);
    update "ores_trading_bond_instruments_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
