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
 * Trade Booking Table
 *
 * A trade's booking: the book that holds it, the netting set it is filed
 * in, the date it was agreed and the time it was executed. It is a
 * component of the [[id:4304A441-E532-45FB-837A-378F13693CAE][trade anchor]], keyed by the trade id, and versions on
 * its own timeline: a book move or a re-papering into another netting set
 * cuts a new version of the booking and leaves the anchor alone.
 *
 * The booking copies the anchor's party and counterparty. The copies are
 * pinned to the anchor, so they cannot drift, and they are what the other
 * pins and row-level security read: the book must belong to the trade's
 * party, and the netting set to its counterparty and party.
 *
 * A booking names a virtual book, one inside a sandbox, only while its
 * trade is a pre-agreement draft, a test or a hypothetical. An actual trade
 * going live needs a real book: the booking is written before the state
 * that takes the trade live, in the same transaction.
 */

create table if not exists "ores_trading_trade_bookings_tbl" (
    "trade_id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "counterparty_id" uuid null,
    "book_id" uuid not null,
    "netting_set_id" uuid null,
    "counterparty_identifier_id" uuid null,
    "netting_set_identifier_id" uuid null,
    "trade_date" date null,
    "execution_timestamp" timestamp with time zone null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    primary key (tenant_id, trade_id, valid_from, valid_to),
    exclude using gist (
        tenant_id WITH =,
        trade_id WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to"),
    check ("trade_id" <> ores_utility_nil_uuid_fn()),
    constraint ores_trading_trade_bookings_trade_id_fk foreign key ("tenant_id", "trade_id") references "ores_trading_trade_anchors_tbl" ("tenant_id", "id"),
    constraint ores_trading_trade_bookings_anchor_party_pin foreign key ("tenant_id", "trade_id", "party_id") references "ores_trading_trade_anchors_tbl" ("tenant_id", "id", "party_id"),
    constraint ores_trading_trade_bookings_anchor_counterparty_pin foreign key ("tenant_id", "trade_id", "counterparty_id") references "ores_trading_trade_anchors_tbl" ("tenant_id", "id", "counterparty_id")
);

-- Version uniqueness for optimistic concurrency
create unique index if not exists trade_bookings_version_uniq_idx
on "ores_trading_trade_bookings_tbl" (tenant_id, trade_id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists trade_bookings_id_uniq_idx
on "ores_trading_trade_bookings_tbl" (tenant_id, trade_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists trade_bookings_tenant_idx
on "ores_trading_trade_bookings_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists trade_bookings_book_idx
on "ores_trading_trade_bookings_tbl" (tenant_id, book_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists trade_bookings_netting_set_idx
on "ores_trading_trade_bookings_tbl" (tenant_id, netting_set_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_trading_trade_bookings_insert_fn()
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

    -- Validate netting_set_id (optional soft FK to ores_refdata_netting_sets_tbl)
    if NEW.netting_set_id is not null then
        if not exists (
            select 1 from ores_refdata_netting_sets_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.netting_set_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid netting_set_id: %. No active netting set found with this id.', NEW.netting_set_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate counterparty_identifier_id (optional soft FK to ores_refdata_counterparty_identifiers_tbl)
    if NEW.counterparty_identifier_id is not null then
        if not exists (
            select 1 from ores_refdata_counterparty_identifiers_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.counterparty_identifier_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid counterparty_identifier_id: %. No active counterparty identifier found with this id.', NEW.counterparty_identifier_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate netting_set_identifier_id (optional soft FK to ores_refdata_netting_set_identifiers_tbl)
    if NEW.netting_set_identifier_id is not null then
        if not exists (
            select 1 from ores_refdata_netting_set_identifiers_tbl
            where tenant_id = NEW.tenant_id
              and id = NEW.netting_set_identifier_id
              and valid_to = ores_utility_infinity_timestamp_fn()
        ) then
            raise exception 'Invalid netting_set_identifier_id: %. No active netting set identifier found with this id.', NEW.netting_set_identifier_id
                using errcode = '23503';
        end if;
    end if;

    -- Validate the book_party pin to ores_refdata_books_tbl
    if NEW.book_id is not null and NEW.party_id is not null and not exists (
        select 1 from ores_refdata_books_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.book_id
          and party_id = NEW.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid book_id: %. The book must belong to the trade''s party.', NEW.book_id
            using errcode = '23503';
    end if;

    -- Validate the netting_set pin to ores_refdata_netting_sets_tbl
    if NEW.netting_set_id is not null and NEW.counterparty_id is not null and NEW.party_id is not null and not exists (
        select 1 from ores_refdata_netting_sets_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.netting_set_id
          and counterparty_id = NEW.counterparty_id
          and party_id = NEW.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid netting_set_id: %. The set must be the trade''s counterparty''s and party''s.', NEW.netting_set_id
            using errcode = '23503';
    end if;

    -- Validate the counterparty_name pin to ores_refdata_counterparty_identifiers_tbl
    if NEW.counterparty_identifier_id is not null and NEW.counterparty_id is not null and not exists (
        select 1 from ores_refdata_counterparty_identifiers_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.counterparty_identifier_id
          and counterparty_id = NEW.counterparty_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid counterparty_identifier_id: %. The identifier must be the trade''s counterparty''s.', NEW.counterparty_identifier_id
            using errcode = '23503';
    end if;

    -- Validate the netting_set_name pin to ores_refdata_netting_set_identifiers_tbl
    if NEW.netting_set_identifier_id is not null and NEW.netting_set_id is not null and not exists (
        select 1 from ores_refdata_netting_set_identifiers_tbl
        where tenant_id = NEW.tenant_id
          and id = NEW.netting_set_identifier_id
          and netting_set_id = NEW.netting_set_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid netting_set_identifier_id: %. The identifier must be the netting set''s.', NEW.netting_set_identifier_id
            using errcode = '23503';
    end if;

    if ores_trading_book_is_virtual_fn(NEW.tenant_id, NEW.book_id)
       and not ores_trading_trade_may_be_virtual_fn(NEW.tenant_id, NEW.trade_id) then
        raise exception 'Invalid book_id: %. A virtual book holds only drafts, tests and hypotheticals.',
            NEW.book_id
            using errcode = '23514';
    end if;
    -- Validate change_reason_code
    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    -- Version management
    select version into current_version
    from "ores_trading_trade_bookings_tbl"
    where tenant_id = NEW.tenant_id
      and trade_id = NEW.trade_id
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
        update "ores_trading_trade_bookings_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = NEW.tenant_id
          and trade_id = NEW.trade_id
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

create or replace trigger ores_trading_trade_bookings_insert_trg
before insert on "ores_trading_trade_bookings_tbl"
for each row execute function ores_trading_trade_bookings_insert_fn();

create or replace rule ores_trading_trade_bookings_delete_rule as
on delete to "ores_trading_trade_bookings_tbl" do instead (
    update "ores_trading_trade_bookings_tbl"
    set valid_to = clock_timestamp()
    where tenant_id = OLD.tenant_id
      and trade_id = OLD.trade_id
      and valid_to = ores_utility_infinity_timestamp_fn();
);
