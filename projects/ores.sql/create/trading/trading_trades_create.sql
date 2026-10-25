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
 * Trade Table
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
 * the last task of the story; until then it is named trade. It has
 * no audit tail: the audit record of the act that booked it carries that.
 */

create table if not exists "ores_trading_trades_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "counterparty_id" uuid null,
    "trade_type" text not null,
    "counterparty_scope" text not null,
    "booking_nature" text not null,
    "entry_channel" text not null,
    "origin" text not null,
    "origin_reference" text null,
    "external_version" integer not null,
    "economic_digest" text null,
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
    check ("counterparty_scope" = 'intra_entity' or "counterparty_id" is not null)
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists trades_version_uniq_idx
on "ores_trading_trades_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists trades_id_uniq_idx
on "ores_trading_trades_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists trades_tenant_idx
on "ores_trading_trades_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists trades_party_idx
on "ores_trading_trades_tbl" (tenant_id, party_id);

create index if not exists trades_counterparty_idx
on "ores_trading_trades_tbl" (tenant_id, counterparty_id);


-- =============================================================================
-- Touch-version function for trade
-- Bumps this entity's version without changing any of its own columns,
-- called by a child entity's insert trigger / delete rule when that
-- child is flagged :bump_parent_version: true against this entity. See
-- the "Temporal composite entity versioning" architecture doc.
--
-- Deliberately does NOT duplicate the version/valid_from/valid_to
-- management here: it fetches the current row, states the version it
-- read, and re-inserts — the table's own insert trigger (below) then
-- performs the exact same close-current/bump-version dance a
-- normal application save does. Stating the version read, rather than
-- the zero sentinel, is what makes the store see a replace: zero means
-- "no current row exists", so a touch that wrote zero over the live row
-- it had just read would be refused as a create that collides. Because
-- the read above holds the row lock, the version it states is still
-- current when the trigger re-reads it, so the compare-and-swap passes
-- for the same reason an application's own stated version does. This
-- also sidesteps a real footgun:
-- current_timestamp is frozen for the whole transaction, so a
-- same-transaction bulk import (parent + child created together, as
-- GLEIF provisioning does) would otherwise make the just-inserted
-- parent row's own valid_from collide with this function's close
-- timestamp.
--
-- Defined before the insert trigger/delete rule below (not after):
-- for a self-referencing entity (this entity is its own composite
-- parent, e.g. a portfolio tree), the delete rule calling this
-- function lives in the *same* file. PostgreSQL parses CREATE RULE
-- eagerly, so the function must already exist by then -- unlike a
-- PL/pgSQL function body (e.g. the insert trigger function), which is
-- opaque at CREATE time and only resolves calls at execution.
--
-- p_reason_code is passed through as-is (already a validated code from
-- the child's own row); p_child_entity distinguishes which child
-- triggered the bump in the free-text commentary.
-- =============================================================================
create or replace function ores_trading_trades_touch_version_fn(
    p_tenant_id uuid,
    p_id uuid,
    p_reason_code text,
    p_commentary text,
    p_modified_by text,
    p_performed_by text,
    p_child_entity text
) returns void as $$
declare
    rec ores_trading_trades_tbl%rowtype;
begin
    -- for update: takes the same row lock the parent's own insert
    -- trigger takes, so the snapshot in rec can't be based on a
    -- business-column value a concurrent direct edit is about to
    -- change — without this, that concurrent edit could be silently
    -- reverted once this function's later insert proceeds. See the
    -- "Temporal composite entity versioning" architecture doc,
    -- Concurrency section.
    select * into rec
    from "ores_trading_trades_tbl"
    where tenant_id = p_tenant_id
      and id = p_id
      and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if not found then
        return;
    end if;

    -- Left at the version just read: the insert trigger's
    -- compare-and-swap needs a replace, not a create.
    rec.modified_by := p_modified_by;
    rec.performed_by := p_performed_by;
    rec.change_reason_code := p_reason_code;
    rec.change_commentary := format('Bumped by child %s: %s', p_child_entity, coalesce(p_commentary, ''));

    insert into "ores_trading_trades_tbl"
    select (rec).*;
end;
$$ language plpgsql security definer set search_path = public, pg_temp;

create or replace function ores_trading_trades_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate origin (soft FK to ores_trading_origin_types_tbl)
    if not exists (
        select 1 from ores_trading_origin_types_tbl
        where code = NEW.origin
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid origin: %. No active origin type found with this code.', NEW.origin
            using errcode = '23503';
    end if;

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

    -- Validate counterparty_scope (soft FK to ores_trading_counterparty_scope_types_tbl)
    if not exists (
        select 1 from ores_trading_counterparty_scope_types_tbl
        where code = NEW.counterparty_scope
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid counterparty_scope: %. No active counterparty scope found with this id.', NEW.counterparty_scope
            using errcode = '23503';
    end if;

    -- Validate booking_nature (soft FK to ores_trading_booking_nature_types_tbl)
    if not exists (
        select 1 from ores_trading_booking_nature_types_tbl
        where code = NEW.booking_nature
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid booking_nature: %. No active booking nature found with this id.', NEW.booking_nature
            using errcode = '23503';
    end if;

    -- Validate entry_channel (soft FK to ores_trading_entry_channel_types_tbl)
    if not exists (
        select 1 from ores_trading_entry_channel_types_tbl
        where code = NEW.entry_channel
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid entry_channel: %. No active entry channel found with this id.', NEW.entry_channel
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
    from "ores_trading_trades_tbl"
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
                perform ores_outcome_raise_fn(
                    'already_exists',
                    'trade',
                    'id');
            end if;
        elsif NEW.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'trade',
                'id',
                NEW.version::text,
                current_version::text);
        end if;
        NEW.version = current_version + 1;
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. a composite entity's parent
        -- touched twice by two different children in one transaction)
        -- would collide with itself. clock_timestamp() always advances.
        update "ores_trading_trades_tbl"
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

create or replace trigger ores_trading_trades_insert_trg
before insert on "ores_trading_trades_tbl"
for each row execute function ores_trading_trades_insert_fn();

create or replace rule ores_trading_trades_delete_rule as
on delete to "ores_trading_trades_tbl" do instead (
    update "ores_trading_trades_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
);

-- =============================================================================
-- Row-level security: tenant isolation for Trade
-- =============================================================================
alter table ores_trading_trades_tbl enable row level security;

drop policy if exists trades_tbl_tenant_isolation_policy
    on ores_trading_trades_tbl;

create policy trades_tbl_tenant_isolation_policy
on ores_trading_trades_tbl
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
