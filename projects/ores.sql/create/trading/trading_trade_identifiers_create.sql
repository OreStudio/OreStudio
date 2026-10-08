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
 * Trade Identifier Table
 *
 * One identifier a trade is known by under one scheme: a UTI, a USI, the
 * firm's internal reference, or the id an ORE document gave it. Each scheme
 * is a [[id:20D446E8-EA13-47AA-BE0C-FDDD7CF428F3][trade id type]] code, and a trade has at most one value per scheme.
 * The firm's own id is the anchor's UUID, the trade id, and is not repeated
 * here.
 *
 * The row is keyed by the trade and references the [[id:4304A441-E532-45FB-837A-378F13693CAE][trade anchor]] with a
 * database foreign key. It copies the anchor's party, pinned to the anchor,
 * because row-level security needs the party on every row. The issuer is
 * the party or counterparty that issued the value, when it is known.
 */

create table if not exists "ores_trading_trade_identifiers_tbl" (
    "trade_id" uuid not null,
    "id_type" text not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "trade_activity_id" uuid not null,
    "party_id" uuid not null,
    "id_value" text not null,
    "issuing_party_id" uuid null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, trade_id, id_type, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        trade_id WITH =,
        id_type WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("trade_id" <> ores_utility_nil_uuid_fn()),
    check ("id_type" <> ''),
    check ("id_value" <> ''),
    constraint ores_trading_trade_identifiers_trade_id_fk foreign key ("tenant_id", "trade_id") references "ores_trading_trades_tbl" ("tenant_id", "id"),
    constraint ores_trading_trade_identifiers_trade_activity_id_fk foreign key ("tenant_id", "trade_activity_id") references "ores_trading_trade_activities_tbl" ("tenant_id", "id"),
    constraint ores_trading_trade_identifiers_anchor_party_pin foreign key ("tenant_id", "trade_id", "party_id") references "ores_trading_trades_tbl" ("tenant_id", "id", "party_id")
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists trade_identifiers_version_uniq_idx
on "ores_trading_trade_identifiers_tbl" (tenant_id, trade_id, id_type, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists trade_identifiers_id_uniq_idx
on "ores_trading_trade_identifiers_tbl" (tenant_id, trade_id, id_type)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists trade_identifiers_tenant_idx
on "ores_trading_trade_identifiers_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists trade_identifiers_value_idx
on "ores_trading_trade_identifiers_tbl" (tenant_id, id_type, id_value)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_trade_identifiers_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate id_type (soft FK to ores_trading_trade_id_types_tbl)
    if not exists (
        select 1 from ores_trading_trade_id_types_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.id_type
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid id_type: %. No active trade id type found with this code.', NEW.id_type
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
    from "ores_trading_trade_identifiers_tbl"
    where tenant_id = NEW.tenant_id
      and trade_id = NEW.trade_id and id_type = NEW.id_type
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
                    'trade_identifier',
                    'trade_id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'trade_identifier',
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
        update "ores_trading_trade_identifiers_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and trade_id = NEW.trade_id and id_type = NEW.id_type
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

create or replace trigger ores_trading_trade_identifiers_insert_trg
before insert on "ores_trading_trade_identifiers_tbl"
for each row execute function ores_trading_trade_identifiers_insert_fn();

create or replace rule ores_trading_trade_identifiers_delete_rule as
on delete to "ores_trading_trade_identifiers_tbl" do instead (
    update "ores_trading_trade_identifiers_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id and id_type = OLD.id_type
      and valid_to = ores_utility_infinity_timestamp_fn();
);
