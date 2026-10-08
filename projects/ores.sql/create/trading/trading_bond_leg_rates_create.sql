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
 * Bond Leg Rate Table
 *
 * One row per leg, carrying the arm of the rate group the document
 * chose and the members that arm states.
 *
 * The ORE schema states the group as a choice of eighteen alternatives.
 * A leg carries a rate, and the corpus states three of them:
 * fixed, floating and formula-based. The other fifteen are a recorded
 * boundary, and a document that states one of them has no row here. The
 * rate_kind column records which arm the document engaged.
 *
 * The three arms share members. All three lead with an index and all
 * three state an arrears flag, and the floating and formula arms both
 * state a fixing-days count. Those three members have one column each
 * and the arm decides how a reader takes them, so a fixed leg's row
 * leaves them unset.
 *
 * Each arm also has members the others lack, and they sit here as
 * nullable columns rather than in a table per arm. The floating arm's
 * sixteen members and the formula arm's fixing calendar are what remain
 * after the shared three and the lists below are taken out.
 *
 * The three lists the floating arm states, its spreads, caps, floors and
 * gearings, are not here: each is a numbered amount and lives in
 * bond_leg_amount under its own role. Its two schedules live in
 * instrument_schedule, under the fixing_schedule and
 * reset_schedule roles.
 *
 * The two stub interpolation blocks are folded into columns here. Each
 * is a pair of indices with an optional rounding, and the schema states
 * the block at most once at each end of the leg.
 */

create table if not exists "ores_trading_bond_leg_rates_tbl" (
    "trade_id" uuid not null,
    "leg_role" text not null,
    "leg_number" integer not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "trade_activity_id" uuid not null,
    "rate_kind" text not null,
    "index" text null,
    "is_in_arrears" boolean null,
    "fixing_days" bigint null,
    "fixing_calendar" text null,
    "last_recent_period" text null,
    "last_recent_period_calendar" text null,
    "lookback" text null,
    "rate_cutoff" bigint null,
    "is_averaged" boolean null,
    "has_sub_periods" boolean null,
    "include_spread" boolean null,
    "is_not_resetting_xccy" boolean null,
    "naked_option" boolean null,
    "local_cap_floor" boolean null,
    "stub_use_original_curve" boolean null,
    "observation_shift" boolean null,
    "front_stub_short_index" text null,
    "front_stub_long_index" text null,
    "front_stub_rounding_type" text null,
    "front_stub_rounding_precision" bigint null,
    "back_stub_short_index" text null,
    "back_stub_long_index" text null,
    "back_stub_rounding_type" text null,
    "back_stub_rounding_precision" bigint null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, trade_id, leg_role, leg_number, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        trade_id WITH =,
        leg_role WITH =,
        leg_number WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("trade_id" <> ores_utility_nil_uuid_fn()),
    check ("leg_role" <> ''),
    check ("rate_kind" in ('fixed', 'floating', 'formula_based')),
    check ("leg_role" in ('trs_funding', 'repo', 'ascot_swap')),
    check ("leg_number" > 0)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists bond_leg_rates_version_uniq_idx
on "ores_trading_bond_leg_rates_tbl" (tenant_id, trade_id, leg_role, leg_number, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists bond_leg_rates_id_uniq_idx
on "ores_trading_bond_leg_rates_tbl" (tenant_id, trade_id, leg_role, leg_number)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists bond_leg_rates_tenant_idx
on "ores_trading_bond_leg_rates_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_bond_leg_rates_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

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
    from "ores_trading_bond_leg_rates_tbl"
    where tenant_id = NEW.tenant_id
      and trade_id = NEW.trade_id and leg_role = NEW.leg_role and leg_number = NEW.leg_number
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
                    'bond_leg_rate',
                    'trade_id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'bond_leg_rate',
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
        update "ores_trading_bond_leg_rates_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and trade_id = NEW.trade_id and leg_role = NEW.leg_role and leg_number = NEW.leg_number
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

create or replace trigger ores_trading_bond_leg_rates_insert_trg
before insert on "ores_trading_bond_leg_rates_tbl"
for each row execute function ores_trading_bond_leg_rates_insert_fn();

create or replace rule ores_trading_bond_leg_rates_delete_rule as
on delete to "ores_trading_bond_leg_rates_tbl" do instead (
    update "ores_trading_bond_leg_rates_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id and leg_role = OLD.leg_role and leg_number = OLD.leg_number
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Bond Leg Rate
-- =============================================================================
alter table ores_trading_bond_leg_rates_tbl enable row level security;

drop policy if exists bond_leg_rates_tbl_tenant_isolation_policy
    on ores_trading_bond_leg_rates_tbl;

create policy bond_leg_rates_tbl_tenant_isolation_policy
on ores_trading_bond_leg_rates_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
