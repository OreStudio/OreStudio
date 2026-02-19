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
 *  Table
 *
 * Operational ledger leaves. The only entity that holds trades.
 * Serves as the basis for accounting, ownership, and regulatory
 * capital treatment. Must belong to exactly one portfolio.
 */

create table if not exists "ores_refdata_books_tbl" (
    "id" uuid not null,
    "tenant_id" uuid not null,
    "version" integer not null,
    "party_id" uuid not null,
    "name" text not null,
    "parent_portfolio_id" uuid not null,
    "ledger_ccy" text not null,
    "gl_account_ref" text null,
    "cost_center" text null,
    "book_status" text not null,
    "is_trading_book" integer not null,
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

-- Composite natural key: unique book name per party within tenant
create unique index if not exists ores_refdata_books_party_name_uniq_idx
on "ores_refdata_books_tbl" (tenant_id, party_id, name)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Version uniqueness for optimistic concurrency
create unique index if not exists ores_refdata_books_version_uniq_idx
on "ores_refdata_books_tbl" (tenant_id, id, version)
where valid_to = ores_utility_infinity_timestamp_fn();

create unique index if not exists ores_refdata_books_id_uniq_idx
on "ores_refdata_books_tbl" (tenant_id, id)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists ores_refdata_books_tenant_idx
on "ores_refdata_books_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_books_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    NEW.tenant_id := ores_iam_validate_tenant_fn(NEW.tenant_id);

    -- Validate parent_portfolio_id (mandatory soft FK)
    if not exists (
        select 1 from ores_refdata_portfolios_tbl
        where tenant_id = NEW.tenant_id and id = NEW.parent_portfolio_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid parent_portfolio_id: %. No active portfolio found with this id.',
            NEW.parent_portfolio_id
            using errcode = '23503';
    end if;

    -- Validate party_id (mandatory soft FK to parties)
    if not exists (
        select 1 from ores_refdata_parties_tbl
        where tenant_id = NEW.tenant_id and id = NEW.party_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid party_id: %. No active party found with this id.',
            NEW.party_id
            using errcode = '23503';
    end if;

    -- Validate ledger_ccy
    NEW.ledger_ccy := ores_refdata_validate_currency_fn(NEW.tenant_id, NEW.ledger_ccy);

    -- Validate book_status
    NEW.book_status := ores_refdata_validate_book_status_fn(NEW.tenant_id, NEW.book_status);

    -- Version management
    select version into current_version
    from "ores_refdata_books_tbl"
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

        update "ores_refdata_books_tbl"
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

    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);
    new.performed_by = current_user;

    NEW.change_reason_code := ores_dq_validate_change_reason_fn(NEW.tenant_id, NEW.change_reason_code);

    return NEW;
end;
$$ language plpgsql;

create or replace trigger ores_refdata_books_insert_trg
before insert on "ores_refdata_books_tbl"
for each row execute function ores_refdata_books_insert_fn();

create or replace rule ores_refdata_books_delete_rule as
on delete to "ores_refdata_books_tbl" do instead
    update "ores_refdata_books_tbl"
    set valid_to = current_timestamp
    where tenant_id = OLD.tenant_id
      and id = OLD.id
      and valid_to = ores_utility_infinity_timestamp_fn();
