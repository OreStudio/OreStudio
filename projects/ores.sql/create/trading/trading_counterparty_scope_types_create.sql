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
 * Counterparty Scope Type Table
 *
 * Reference data table holding who the firm faces on a trade, and so what
 * obligation the booking creates: external (a party outside the group),
 * inter_entity (another legal entity of the same group) and
 * intra_entity (another book in the same legal entity and branch).
 *
 * The set is closed, and the C++ enum domain::counterparty_scope holds the same
 * values. A code with no enum value cannot be read back, so the table is
 * immutable: it is seeded once and never changes at run time. Because it
 * is immutable, the trade anchor references it with a database foreign
 * key rather than a trigger check. The table has no tenant: the set is the
 * same for every tenant.
 */

create table if not exists "ores_trading_counterparty_scope_types_tbl" (
    "code" text not null,
    "description" text not null,
    primary key (code),
    check ("code" <> '')
);



create or replace function ores_trading_counterparty_scope_types_insert_fn()
returns trigger as $$
declare
begin


    return NEW;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace trigger ores_trading_counterparty_scope_types_insert_trg
before insert on "ores_trading_counterparty_scope_types_tbl"
for each row execute function ores_trading_counterparty_scope_types_insert_fn();

create or replace function ores_trading_counterparty_scope_types_immutable_fn()
returns trigger as $$
begin
    -- A tenant purge is the one sanctioned delete. It turns the switch on
    -- for its own transaction and off again after its delete.
    if TG_OP = 'DELETE' and ores_utility_immutable_purge_allowed_fn() then
        return OLD;
    end if;
    raise exception 'ores_trading_counterparty_scope_types_tbl rows are immutable: % is refused.', TG_OP
        using errcode = '55000';
end;
$$ language plpgsql set search_path = public, pg_temp;

create or replace trigger ores_trading_counterparty_scope_types_immutable_trg
before update or delete on "ores_trading_counterparty_scope_types_tbl"
for each row execute function ores_trading_counterparty_scope_types_immutable_fn();

create or replace trigger ores_trading_counterparty_scope_types_immutable_truncate_trg
before truncate on "ores_trading_counterparty_scope_types_tbl"
for each statement execute function ores_trading_counterparty_scope_types_immutable_fn();

