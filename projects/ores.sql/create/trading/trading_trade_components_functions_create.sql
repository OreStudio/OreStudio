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
-- Trade component helpers
-- =============================================================================

-- The checks the trade booking and the trade state share. A virtual book, one
-- inside a sandbox, holds no position of the firm, so an actual trade may sit in
-- one only while it is a draft. Whichever of the two rows is written second
-- checks the other, so the order of the writes in a transaction does not matter
-- for the rule, only for which row reports a breach.

-- Whether a book is virtual: it belongs to a sandbox.
create or replace function ores_trading_book_is_virtual_fn(
    p_tenant_id uuid,
    p_book_id   uuid
)
returns boolean as $$
begin
    return exists (
        select 1 from ores_refdata_books_tbl
        where tenant_id = p_tenant_id
          and id = p_book_id
          and sandbox_id is not null
          and valid_to = ores_utility_infinity_timestamp_fn()
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether a status is the draft state of the trade_status machine.
create or replace function ores_trading_status_is_draft_fn(
    p_status_id uuid
)
returns boolean as $$
begin
    return exists (
        select 1
        from ores_dq_fsm_states_tbl s
        join ores_dq_fsm_machines_tbl m
          on m.tenant_id = s.tenant_id
         and m.id = s.machine_id
         and m.valid_to = ores_utility_infinity_timestamp_fn()
        where s.tenant_id = ores_utility_system_tenant_id_fn()
          and s.id = p_status_id
          and s.name = 'draft'
          and m.name = 'trade_status'
          and s.valid_to = ores_utility_infinity_timestamp_fn()
    );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether an actual trade in the given status may sit in a virtual book. A
-- test or a hypothetical always may; an actual trade only while it is a draft.
create or replace function ores_trading_status_may_be_virtual_fn(
    p_tenant_id uuid,
    p_trade_id  uuid,
    p_status_id uuid
)
returns boolean as $$
begin
    return ores_trading_status_is_draft_fn(p_status_id)
        or exists (
            select 1 from ores_trading_trade_anchors_tbl
            where tenant_id = p_tenant_id
              and id = p_trade_id
              and booking_nature in ('test', 'hypothetical')
        );
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether a trade may sit in a virtual book given its current state. A trade
-- with no state yet may: the state written after it checks the booking.
create or replace function ores_trading_trade_may_be_virtual_fn(
    p_tenant_id uuid,
    p_trade_id  uuid
)
returns boolean as $$
declare
    v_status_id uuid;
begin
    select status_id into v_status_id
    from ores_trading_trade_states_tbl
    where tenant_id = p_tenant_id
      and trade_id = p_trade_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    if not found then
        return true;
    end if;
    return ores_trading_status_may_be_virtual_fn(p_tenant_id, p_trade_id, v_status_id);
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- Whether a trade's current booking names a virtual book.
create or replace function ores_trading_trade_booked_virtual_fn(
    p_tenant_id uuid,
    p_trade_id  uuid
)
returns boolean as $$
declare
    v_book_id uuid;
begin
    select book_id into v_book_id
    from ores_trading_trade_bookings_tbl
    where tenant_id = p_tenant_id
      and trade_id = p_trade_id
      and valid_to = ores_utility_infinity_timestamp_fn();

    return found and ores_trading_book_is_virtual_fn(p_tenant_id, v_book_id);
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;

-- =============================================================================
-- Book a trade
-- =============================================================================

-- Books a trade: writes its anchor, its booking and its first state in one
-- statement, so a failure in any of them writes none. The booking and the
-- state take their trade id and party from the anchor, and the booking its
-- counterparty. The booking is written before the state, so a draft may sit in
-- a virtual book and a live actual trade may not.
--
-- Returns false, and writes nothing, when the trade id is already booked: the
-- anchor is written once, and a second booking of it is a conflict for the
-- caller to report rather than a key violation. Runs as the caller, so the
-- caller's grants and row-level security apply to every write.
create or replace function ores_trading_book_trade_fn(
    p_id                  uuid,
    p_party_id            uuid,
    p_counterparty_id     uuid,
    p_trade_type          text,
    p_counterparty_scope  text,
    p_booking_nature      text,
    p_entry_channel       text,
    p_book_id             uuid,
    p_netting_set_id      uuid,
    p_counterparty_identifier_id uuid,
    p_netting_set_identifier_id  uuid,
    p_trade_date          date,
    p_execution_timestamp timestamptz,
    p_activity_type_code  text,
    p_modified_by         text,
    p_change_reason_code  text,
    p_change_commentary   text
)
returns boolean as $$
declare
    v_tenant_id uuid := ores_iam_current_tenant_id_fn();
begin
    insert into ores_trading_trade_anchors_tbl (id, tenant_id, party_id, counterparty_id,
        trade_type, counterparty_scope, booking_nature, entry_channel)
    values (p_id, v_tenant_id, p_party_id, p_counterparty_id, p_trade_type,
        p_counterparty_scope, p_booking_nature, p_entry_channel)
    on conflict (tenant_id, id) do nothing;

    if not found then
        return false;
    end if;

    insert into ores_trading_trade_bookings_tbl (trade_id, tenant_id, version, party_id,
        counterparty_id, book_id, netting_set_id, counterparty_identifier_id,
        netting_set_identifier_id, trade_date, execution_timestamp,
        modified_by, performed_by, change_reason_code, change_commentary)
    values (p_id, v_tenant_id, 0, p_party_id, p_counterparty_id, p_book_id,
        p_netting_set_id, p_counterparty_identifier_id, p_netting_set_identifier_id,
        p_trade_date, p_execution_timestamp, p_modified_by,
        p_modified_by, p_change_reason_code, p_change_commentary);

    insert into ores_trading_trade_states_tbl (trade_id, tenant_id, version, party_id,
        activity_type_code, status_id, modified_by, performed_by, change_reason_code,
        change_commentary)
    values (p_id, v_tenant_id, 0, p_party_id, p_activity_type_code,
        ores_utility_nil_uuid_fn(), p_modified_by, p_modified_by, p_change_reason_code,
        p_change_commentary);

    return true;
end;
$$ language plpgsql set search_path = public, pg_temp;

-- =============================================================================
-- Envelope names
-- =============================================================================

-- The names an ORE envelope gives a booked trade's counterparty and netting
-- set. Each is the identifier the trade's source named it by, else the
-- entity's ORE alias, else its own code; a trade with no counterparty or no
-- netting set gets no name for it. Runs as the owner, because the names live
-- in refdata tables the trading service does not read; every row is held to
-- the caller's tenant, and the caller passes only trades it has read.
create or replace function ores_trading_trade_envelope_names_fn(p_trade_ids uuid[])
returns table (trade_id uuid, counter_party text, netting_set_id text) as $$
#variable_conflict use_column
begin
    return query
    select a.id,
        case when a.counterparty_id is not null then coalesce(
            (select ci.id_value from ores_refdata_counterparty_identifiers_tbl ci
             where ci.tenant_id = a.tenant_id
               and ci.id = b.counterparty_identifier_id
               and ci.valid_to = ores_utility_infinity_timestamp_fn()),
            (select ci.id_value from ores_refdata_counterparty_identifiers_tbl ci
             where ci.tenant_id = a.tenant_id
               and ci.counterparty_id = a.counterparty_id
               and ci.id_scheme = 'ORE'
               and ci.valid_to = ores_utility_infinity_timestamp_fn()
             order by ci.id_value
             limit 1),
            (select cp.short_code from ores_refdata_counterparties_tbl cp
             where cp.tenant_id = a.tenant_id
               and cp.id = a.counterparty_id
               and cp.valid_to = ores_utility_infinity_timestamp_fn())) end,
        case when b.netting_set_id is not null then coalesce(
            (select nsi.id_value from ores_refdata_netting_set_identifiers_tbl nsi
             where nsi.tenant_id = a.tenant_id
               and nsi.id = b.netting_set_identifier_id
               and nsi.valid_to = ores_utility_infinity_timestamp_fn()),
            (select nsi.id_value from ores_refdata_netting_set_identifiers_tbl nsi
             where nsi.tenant_id = a.tenant_id
               and nsi.netting_set_id = b.netting_set_id
               and nsi.id_scheme = 'ORE'
               and nsi.valid_to = ores_utility_infinity_timestamp_fn()
             order by nsi.id_value
             limit 1),
            (select ns.code from ores_refdata_netting_sets_tbl ns
             where ns.tenant_id = a.tenant_id
               and ns.id = b.netting_set_id
               and ns.valid_to = ores_utility_infinity_timestamp_fn())) end
    from ores_trading_trade_anchors_tbl a
    join ores_trading_trade_bookings_tbl b
      on b.tenant_id = a.tenant_id
     and b.trade_id = a.id
     and b.valid_to = ores_utility_infinity_timestamp_fn()
    where a.tenant_id = ores_iam_current_tenant_id_fn()
      and a.id = any(p_trade_ids);
end;
$$ language plpgsql stable security definer set search_path = public, pg_temp;
