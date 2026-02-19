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
 * Trade Envelope Table
 *
 * Temporal trade envelope capturing FpML Trade Header and ORE Envelope
 * properties. Each lifecycle event (New, Amendment, Novation, etc.) creates
 * a new temporal row for the same trade id. The internal party is derived
 * from book_id -> books.party_id.
 *
 * Hand-crafted: inline soft FK validations for book_id, portfolio_id, and
 * successor_trade_id cannot be expressed by the standard templates.
 */

create table if not exists "ores_trade_trades_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "external_id" text null,
    "book_id" uuid not null,
    "portfolio_id" uuid not null,
    "successor_trade_id" uuid null,
    "trade_type" text not null,
    "netting_set_id" text not null,
    "lifecycle_event" text not null,
    "trade_date" date not null,
    "execution_timestamp" timestamp with time zone not null,
    "effective_date" date not null,
    "termination_date" date not null,
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
    check ("id" <> '00000000-0000-0000-0000-000000000000'::uuid)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists ores_trade_trades_version_uniq_idx
on "ores_trade_trades_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Current record uniqueness
create unique index if not exists ores_trade_trades_id_uniq_idx
on "ores_trade_trades_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Tenant index
create index if not exists ores_trade_trades_tenant_idx
on "ores_trade_trades_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Book index for portfolio lookups
create index if not exists ores_trade_trades_book_idx
on "ores_trade_trades_tbl" (tenant_id, book_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Portfolio index
create index if not exists ores_trade_trades_portfolio_idx
on "ores_trade_trades_tbl" (tenant_id, portfolio_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Netting set index for ORE aggregation
create index if not exists ores_trade_trades_netting_set_idx
on "ores_trade_trades_tbl" (tenant_id, netting_set_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Trade type index for product filtering
create index if not exists ores_trade_trades_trade_type_idx
on "ores_trade_trades_tbl" (tenant_id, trade_type)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trade_trades_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate book_id (soft FK to ores_refdata_books_tbl)
    if not exists (
        select 1 from ores_refdata_books_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.book_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid book_id: %. Book must exist for tenant.', NEW.book_id
            using errcode = '23503';
    end if;

    -- Validate portfolio_id (soft FK to ores_refdata_portfolios_tbl)
    if not exists (
        select 1 from ores_refdata_portfolios_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.portfolio_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid portfolio_id: %. Portfolio must exist for tenant.', NEW.portfolio_id
            using errcode = '23503';
    end if;

    -- Validate successor_trade_id (optional self-referencing soft FK)
    if NEW.successor_trade_id is not null then
        if not exists (
            select 1 from ores_trade_trades_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.successor_trade_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid successor_trade_id: %. Trade must exist for tenant.', NEW.successor_trade_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate trade_type
    NEW.trade_type := ores_trade_validate_trade_type_fn(NEW.tenant_id, NEW.trade_type);

    -- Validate lifecycle_event
    NEW.lifecycle_event := ores_trade_validate_lifecycle_event_fn(NEW.tenant_id, NEW.lifecycle_event);

    -- Version management
    select version into current_version
    from "ores_trade_trades_tbl"
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

        update "ores_trade_trades_tbl"
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
    NEW.performed_by = current_user;

    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_trade_trades_insert_trg
before insert on "ores_trade_trades_tbl"
for each row execute function ores_trade_trades_insert_fn();

create or replace rule ores_trade_trades_delete_rule as
on delete to "ores_trade_trades_tbl" do instead
    update "ores_trade_trades_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
