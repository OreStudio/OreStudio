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
 * Commodity Basket Constituent Table
 *
 * One row per underlying a commodity basket product names, keyed to the
 * instrument and the constituent's ordinal within the document.
 *
 * The ORE schema states a commodity basket's members as an unbounded list of
 * Underlying elements, each carrying a Name and an optional Weight.
 * The list order is the document's order and the ordinal preserves it, so
 * export re-emits the constituents as the document held them.
 *
 * A constituent is a name and, when the document states one, a weight. The
 * weight is money-role decimal, so the column holds an exact numeric and not
 * a binary float. The document may state no weight, and then the column is
 * null rather than a zero the document never wrote.
 *
 * The trade row carries the workspace and the party. The
 * constituent rows are family-owned and ride the trade's scope, so no
 * workspace column rides them.
 */

create table if not exists "ores_trading_commodity_basket_constituents_tbl" (
    "trade_id" uuid not null,
    "sequence_number" integer not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "underlying_code" text not null,
    "weight" numeric(28, 10) null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, trade_id, sequence_number, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        trade_id WITH =,
        sequence_number WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("trade_id" <> ores_utility_nil_uuid_fn()),
    check ("sequence_number" > 0),
    check ("underlying_code" <> '')
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists commodity_basket_constituents_version_uniq_idx
on "ores_trading_commodity_basket_constituents_tbl" (tenant_id, trade_id, sequence_number, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists commodity_basket_constituents_id_uniq_idx
on "ores_trading_commodity_basket_constituents_tbl" (tenant_id, trade_id, sequence_number)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists commodity_basket_constituents_tenant_idx
on "ores_trading_commodity_basket_constituents_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_commodity_basket_constituents_insert_fn()
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

    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_trading_commodity_basket_constituents_tbl"
    where tenant_id = NEW.tenant_id
      and trade_id = NEW.trade_id and sequence_number = NEW.sequence_number
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
        update "ores_trading_commodity_basket_constituents_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and trade_id = NEW.trade_id and sequence_number = NEW.sequence_number
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

create or replace trigger ores_trading_commodity_basket_constituents_insert_trg
before insert on "ores_trading_commodity_basket_constituents_tbl"
for each row execute function ores_trading_commodity_basket_constituents_insert_fn();

create or replace rule ores_trading_commodity_basket_constituents_delete_rule as
on delete to "ores_trading_commodity_basket_constituents_tbl" do instead (
    update "ores_trading_commodity_basket_constituents_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id and sequence_number = OLD.sequence_number
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Commodity Basket Constituent
-- =============================================================================
alter table ores_trading_commodity_basket_constituents_tbl enable row level security;

drop policy if exists commodity_basket_constituents_tbl_tenant_isolation_policy
    on ores_trading_commodity_basket_constituents_tbl;

create policy commodity_basket_constituents_tbl_tenant_isolation_policy
on ores_trading_commodity_basket_constituents_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
