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
 * Swap Leg Table
 *
 * The shared legs table of the nine rates instrument families. Each row is one
 * leg of an FRA, vanilla swap, cap/floor, swaption, balance-guaranteed swap,
 * callable swap, knock-out swap, inflation swap or RPA instrument. A plain
 * interest rate swap has two rows, one fixed and one floating; a cross-currency
 * swap has two rows with different currencies.
 *
 * leg_type_code is the discriminator and the leg type is data, not a table, so
 * one model covers the shared table (story 0DC1BAC7, decision D10). The fields a
 * leg type does not state are null: fixed_rate is null for a floating leg and
 * floating_index_code is null for a fixed leg.
 *
 * The row keeps its own id surrogate and names the parent through trade_id.
 * The instrument is keyed by its trade, so all nine rates families name the one
 * parent the trades table holds rather than a table per family.
 *
 * It binds :profile: trading-instrument, like the nine instrument sub-types
 * whose legs it holds. Two table features justify the bind: the table is
 * tenant-scoped through tenant_id and the tenant isolation policy, its insert trigger stamps
 * party_id from the session variable app.current_party_id rather than taking
 * it from the client. The bind leaves the table with no UI surface -- the
 * per-instrument forms were hand-crafted in the removed desktop client and
 * consumed the generated messaging protocol. The identity and audit field
 * groups and the generator facet are the model's own flags, stated under C++
 * below, not profile assignments.
 *
 * The table is bi-temporal and audited, so the model takes the ordinary audited
 * shape and needs no shape flag.
 */

create table if not exists "ores_trading_swap_legs_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "trade_id" uuid not null,
    "trade_activity_id" uuid not null,
    "leg_number" integer not null default 1,
    "leg_type_code" text not null,
    "day_count_fraction_code" text not null,
    "business_day_convention_code" text not null,
    "payment_frequency_code" text not null,
    "floating_index_code" text null,
    "fixed_rate" numeric(18, 10) null,
    "spread" numeric(18, 10) null,
    "notional" numeric(28, 10) not null,
    "currency" text not null,
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
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("leg_number" >= 1),
    check ("notional" > 0),
    check ("currency" <> ''),
    constraint ores_trading_swap_legs_trade_activity_id_fk foreign key ("tenant_id", "trade_activity_id") references "ores_trading_trade_activities_tbl" ("tenant_id", "id")
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists swap_legs_version_uniq_idx
on "ores_trading_swap_legs_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists swap_legs_id_uniq_idx
on "ores_trading_swap_legs_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists swap_legs_tenant_idx
on "ores_trading_swap_legs_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists swap_legs_party_idx
on "ores_trading_swap_legs_tbl" (tenant_id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists swap_legs_trade_id_idx
on "ores_trading_swap_legs_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_swap_legs_insert_fn()
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

    -- Validate leg_type_code
    NEW.leg_type_code := ores_refdata_validate_leg_type_fn(NEW.tenant_id, NEW.leg_type_code);

    -- Validate day_count_fraction_code
    NEW.day_count_fraction_code := ores_refdata_validate_day_count_fraction_type_fn(NEW.tenant_id, NEW.day_count_fraction_code);

    -- Validate business_day_convention_code
    NEW.business_day_convention_code := ores_refdata_validate_business_day_convention_type_fn(NEW.tenant_id, NEW.business_day_convention_code);

    -- Validate payment_frequency_code
    NEW.payment_frequency_code := ores_refdata_validate_payment_frequency_fn(NEW.tenant_id, NEW.payment_frequency_code);

    -- Validate floating_index_code (optional field -- skip validation when null)
    if NEW.floating_index_code is not null then
        NEW.floating_index_code := ores_refdata_validate_floating_index_type_fn(NEW.tenant_id, NEW.floating_index_code);
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- The actor is validated before the version management and any parent
    -- touch below: the validator accepts a username only while a current
    -- account row holds it, and a self write retires that row.
    NEW.modified_by := ores_iam_validate_account_username_fn(NEW.modified_by);

    -- Version management
    select version into current_version
    from "ores_trading_swap_legs_tbl"
    where tenant_id = NEW.tenant_id
      and id = NEW.id
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
        update "ores_trading_swap_legs_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and id = NEW.id
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

create or replace trigger ores_trading_swap_legs_insert_trg
before insert on "ores_trading_swap_legs_tbl"
for each row execute function ores_trading_swap_legs_insert_fn();

create or replace rule ores_trading_swap_legs_delete_rule as
on delete to "ores_trading_swap_legs_tbl" do instead (
    update "ores_trading_swap_legs_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
