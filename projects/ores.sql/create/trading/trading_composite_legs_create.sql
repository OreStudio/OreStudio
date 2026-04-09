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
-- Composite Legs Table
--
-- Child table of ores_trading_composite_instruments_tbl. Each row is one
-- constituent trade of a composite instrument (CompositeTrade, MultiLegOption).
-- The leg_sequence field provides 1-based ordering of the constituent trades.
-- =============================================================================

create table if not exists "ores_trading_composite_legs_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "instrument_id" uuid not null,
    "leg_sequence" integer not null,
    "constituent_trade_id" text not null,
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
    check ("leg_sequence" >= 1)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists ores_trading_composite_legs_version_uniq_idx
on "ores_trading_composite_legs_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Current record uniqueness
create unique index if not exists ores_trading_composite_legs_id_uniq_idx
on "ores_trading_composite_legs_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Tenant index
create index if not exists ores_trading_composite_legs_tenant_idx
on "ores_trading_composite_legs_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Party index for party isolation
create index if not exists ores_trading_composite_legs_party_idx
on "ores_trading_composite_legs_tbl" (tenant_id, party_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Instrument index for leg lookups
create index if not exists ores_trading_composite_legs_instrument_idx
on "ores_trading_composite_legs_tbl" (tenant_id, instrument_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_composite_legs_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Set party_id from session context
    NEW.party_id := current_setting('app.current_party_id')::uuid;

    -- Validate instrument_id (soft FK to ores_trading_composite_instruments_tbl)
    if not exists (
        select 1 from ores_trading_composite_instruments_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.instrument_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid instrument_id: %. Composite instrument must exist for tenant.', NEW.instrument_id
            using errcode = '23503';
    end if;

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_trading_composite_legs_tbl"
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

        update "ores_trading_composite_legs_tbl"
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

create or replace trigger ores_trading_composite_legs_insert_trg
before insert on "ores_trading_composite_legs_tbl"
for each row execute function ores_trading_composite_legs_insert_fn();

create or replace rule ores_trading_composite_legs_delete_rule as
on delete to "ores_trading_composite_legs_tbl" do instead
    update "ores_trading_composite_legs_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
