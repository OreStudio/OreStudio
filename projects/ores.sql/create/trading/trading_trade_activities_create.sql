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
 * Trade Activity Table
 *
 * One business event on a trade: what kind of event it was, who did it, when
 * it happened, why, and whether it corrects an operational error. An
 * activity is written once and never changed. Every component version names
 * the activity that caused it, so a version's cause is a row rather than a
 * code copied onto each table, and the axes the
 * [[id:9695568B-0A54-4DEE-952A-EC284ED635C7][activity type]] states (economic, confirmable, real, priority) are read
 * from one place.
 *
 * A null amend is an activity like any other: it is recorded, and because
 * its type is not real it versions nothing.
 */

create table if not exists "ores_trading_trade_activities_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "activity_type_code" text not null,
    "actor" text not null,
    "occurred_at" timestamp with time zone not null,
    "comment" text not null,
    "is_operational_error" boolean not null default false,
    primary key (tenant_id, id),
    check ("id" <> ores_utility_nil_uuid_fn())
);



create index if not exists trade_activities_party_idx
on "ores_trading_trade_activities_tbl" (tenant_id, party_id);

create index if not exists trade_activities_activity_type_idx
on "ores_trading_trade_activities_tbl" (tenant_id, activity_type_code);

create index if not exists trade_activities_occurred_at_idx
on "ores_trading_trade_activities_tbl" (tenant_id, occurred_at);

create or replace function ores_trading_trade_activities_insert_fn()
returns trigger as $$
declare
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate party_id (soft FK to ores_refdata_parties_tbl)
    if not exists (
        select 1 from ores_refdata_parties_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid party_id: %. No active party found with this id.', NEW.party_id
            using errcode = '23503';
    end if;

    -- Validate activity_type_code (soft FK to ores_trading_activity_types_tbl)
    if not exists (
        select 1 from ores_trading_activity_types_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.activity_type_code
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid activity_type_code: %. No active activity type found with this code.', NEW.activity_type_code
            using errcode = '23503';
    end if;



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_trading_trade_activities_insert_trg
before insert on "ores_trading_trade_activities_tbl"
for each row execute function ores_trading_trade_activities_insert_fn();

create or replace function ores_trading_trade_activities_immutable_fn()
returns trigger as $$
begin
    -- A tenant purge is the one sanctioned delete. It turns the switch on
    -- for its own transaction and off again after its delete.
    if TG_OP = 'DELETE' and ores_utility_immutable_purge_allowed_fn() then
        return OLD;
    end if;
    raise exception 'ores_trading_trade_activities_tbl rows are immutable: % is refused.', TG_OP
        using errcode = '55000';
end;
$$ language plpgsql set search_path = public, pg_temp;

create or replace trigger ores_trading_trade_activities_immutable_trg
before update or delete on "ores_trading_trade_activities_tbl"
for each row execute function ores_trading_trade_activities_immutable_fn();

create or replace trigger ores_trading_trade_activities_immutable_truncate_trg
before truncate on "ores_trading_trade_activities_tbl"
for each statement execute function ores_trading_trade_activities_immutable_fn();

