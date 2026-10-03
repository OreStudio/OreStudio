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
 * Trade Anchor Table
 *
 * The immutable anchor of a trade. It holds only the facts that are fixed
 * for the trade's life: the firm's legal entity (party), the counterparty,
 * the trade type, and the three closed classifications — counterparty
 * scope, booking nature and entry channel. Everything that changes lives in
 * component tables keyed by the trade, each on its own timeline.
 *
 * A row is written once and never changed. A change of party or
 * counterparty is a cancel and rebook: a new trade, linked to the old one.
 * Because the row never changes, the component tables reference it with
 * database foreign keys instead of trigger checks against temporal rows.
 *
 * The anchor is built beside the wide trade table, which it replaces in
 * the last task of the story; until then it is named trade_anchor. It has
 * no audit tail: the audit record of the act that booked it carries that.
 */

create table if not exists "ores_trading_trade_anchors_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "party_id" uuid not null,
    "counterparty_id" uuid null,
    "trade_type" text not null,
    "counterparty_scope" text not null,
    "booking_nature" text not null,
    "entry_channel" text not null,
    primary key (tenant_id, id),
    check ("id" <> ores_utility_nil_uuid_fn()),
    check ("counterparty_scope" = 'intra_entity' or "counterparty_id" is not null),
    constraint ores_trading_trade_anchors_counterparty_scope_fk foreign key ("counterparty_scope") references "ores_trading_counterparty_scope_types_tbl" ("code"),
    constraint ores_trading_trade_anchors_booking_nature_fk foreign key ("booking_nature") references "ores_trading_booking_nature_types_tbl" ("code"),
    constraint ores_trading_trade_anchors_entry_channel_fk foreign key ("entry_channel") references "ores_trading_entry_channel_types_tbl" ("code")
);



create index if not exists trade_anchors_party_idx
on "ores_trading_trade_anchors_tbl" (tenant_id, party_id);

create index if not exists trade_anchors_counterparty_idx
on "ores_trading_trade_anchors_tbl" (tenant_id, counterparty_id);

create unique index if not exists trade_anchors_id_party_idx
on "ores_trading_trade_anchors_tbl" (tenant_id, id, party_id);

create unique index if not exists trade_anchors_id_counterparty_idx
on "ores_trading_trade_anchors_tbl" (tenant_id, id, counterparty_id);

create or replace function ores_trading_trade_anchors_insert_fn()
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

    -- Validate counterparty_id (optional soft FK to ores_refdata_counterparties_tbl)
    if NEW.counterparty_id is not null then
        if not exists (
            select 1 from ores_refdata_counterparties_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.counterparty_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid counterparty_id: %. Counterparty must exist for tenant.', NEW.counterparty_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate trade_type (soft FK to ores_trading_trade_types_tbl)
    if not exists (
        select 1 from ores_trading_trade_types_tbl
        where tenant_id = ores_utility_system_tenant_id_fn()
          and code = NEW.trade_type
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid trade_type: %. No active trade type found with this code.', NEW.trade_type
            using errcode = '23503';
    end if;



    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_trading_trade_anchors_insert_trg
before insert on "ores_trading_trade_anchors_tbl"
for each row execute function ores_trading_trade_anchors_insert_fn();

create or replace function ores_trading_trade_anchors_immutable_fn()
returns trigger as $$
begin
    -- A tenant purge is the one sanctioned delete. It turns the switch on
    -- for its own transaction and off again after its delete.
    if TG_OP = 'DELETE' and ores_utility_immutable_purge_allowed_fn() then
        return OLD;
    end if;
    raise exception 'ores_trading_trade_anchors_tbl rows are immutable: % is refused.', TG_OP
        using errcode = '55000';
end;
$$ language plpgsql set search_path = public, pg_temp;

create or replace trigger ores_trading_trade_anchors_immutable_trg
before update or delete on "ores_trading_trade_anchors_tbl"
for each row execute function ores_trading_trade_anchors_immutable_fn();

create or replace trigger ores_trading_trade_anchors_immutable_truncate_trg
before truncate on "ores_trading_trade_anchors_tbl"
for each statement execute function ores_trading_trade_anchors_immutable_fn();

