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
 * Trade Identifiers Table
 *
 * Junction table for trade external identifiers (UTI, USI, Internal).
 * Each trade can have multiple identifiers of different types.
 * issuing_party_id validates against both parties and counterparties
 * tables since UUIDs are globally unique.
 *
 * Hand-crafted: inline soft FK validations for trade_id, issuing_party_id
 * cannot be expressed by the standard templates.
 */

create table if not exists "ores_trade_identifiers_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "trade_id" uuid not null,
    "issuing_party_id" uuid null,
    "id_value" text not null,
    "id_type" text not null,
    "id_scheme" text null,
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
create unique index if not exists ores_trade_identifiers_version_uniq_idx
on "ores_trade_identifiers_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Current record uniqueness
create unique index if not exists ores_trade_identifiers_id_uniq_idx
on "ores_trade_identifiers_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Tenant index
create index if not exists ores_trade_identifiers_tenant_idx
on "ores_trade_identifiers_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Trade index for lookups
create index if not exists ores_trade_identifiers_trade_idx
on "ores_trade_identifiers_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Identifier type + value for UTI/USI lookups
create index if not exists ores_trade_identifiers_type_value_idx
on "ores_trade_identifiers_tbl" (tenant_id, id_type, id_value)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trade_identifiers_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate trade_id (soft FK to ores_trade_trades_tbl)
    if not exists (
        select 1 from ores_trade_trades_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.trade_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid trade_id: %. Trade must exist for tenant.', NEW.trade_id
            using errcode = '23503';
    end if;

    -- Validate issuing_party_id (optional, soft FK to parties or counterparties)
    if NEW.issuing_party_id is not null then
        if not exists (
            select 1 from ores_refdata_parties_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.issuing_party_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) and not exists (
            select 1 from ores_refdata_counterparties_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.issuing_party_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid issuing_party_id: %. Must exist in parties or counterparties for tenant.', NEW.issuing_party_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate id_type
    NEW.id_type := ores_trade_validate_trade_id_type_fn(NEW.tenant_id, NEW.id_type);

    -- Version management
    select version into current_version
    from "ores_trade_identifiers_tbl"
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

        update "ores_trade_identifiers_tbl"
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

create or replace trigger ores_trade_identifiers_insert_trg
before insert on "ores_trade_identifiers_tbl"
for each row execute function ores_trade_identifiers_insert_fn();

create or replace rule ores_trade_identifiers_delete_rule as
on delete to "ores_trade_identifiers_tbl" do instead
    update "ores_trade_identifiers_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
