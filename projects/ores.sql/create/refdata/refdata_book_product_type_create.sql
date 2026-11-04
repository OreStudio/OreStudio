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
 * Template: sql_schema_junction_create.mustache
 * To modify, update the template and regenerate.
 *
 * Book Product Type Table
 *
 * The product types a book allows a trade to be booked under.
 *
 * A book either states its allowed set or allows everything, and the two are
 * different facts:
 *
 * - ores_refdata_books_tbl.restricts_product_types is false, which is every
 *   book a tenant creates without saying otherwise: the book allows every product
 *   type the catalogue states, now and later, and this junction holds no rows for
 *   it.
 * - The flag is true: this junction holds the allowed set, and a book with no
 *   rows here allows nothing. An empty set is a decision, not a gap.
 *
 * The distinction matters because the same absence of rows means opposite things.
 * The booking path reads the flag first, so an unrestricted book never queries
 * this junction.
 *
 * The product type is named by the code the catalogue states, the same soft
 * reference the instrument tables use, because the catalogue belongs to the
 * trading component and this junction to refdata: a foreign key would point the
 * reference data at the trades.
 */

create table if not exists "ores_refdata_book_product_types_tbl" (
    "book_id" uuid not null,
    "tenant_id" uuid not null,
    "product_type_code" text not null,
    "version" integer not null,
    "modified_by" text not null,
    "performed_by" text not null,
    "change_reason_code" text not null,
    "change_commentary" text not null,
    "valid_from" timestamp with time zone not null,
    "valid_to" timestamp with time zone not null,
    "closed_by" text null,
    primary key (tenant_id, book_id, product_type_code, valid_from),
    exclude using gist (
        tenant_id WITH =,
        book_id WITH =,
        product_type_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    ),
    check ("valid_from" < "valid_to")
);

-- Index for looking up the product types a book allows
create index if not exists book_product_types_book_idx
on "ores_refdata_book_product_types_tbl" (book_id)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Index for finding the books that allow a product type
create index if not exists book_product_types_product_type_idx
on "ores_refdata_book_product_types_tbl" (product_type_code)
where valid_to = ores_utility_infinity_timestamp_fn();

-- Unique constraint on active records for ON CONFLICT support
create unique index if not exists book_product_types_uniq_idx
on "ores_refdata_book_product_types_tbl" (tenant_id, book_id, product_type_code)
where valid_to = ores_utility_infinity_timestamp_fn();

create index if not exists book_product_types_tenant_idx
on "ores_refdata_book_product_types_tbl" (tenant_id)
where valid_to = ores_utility_infinity_timestamp_fn();

create or replace function ores_refdata_book_product_types_insert_fn()
returns trigger as $$
declare
    current_version integer;
begin
    -- Validate tenant_id
    new.tenant_id := ores_iam_validate_tenant_fn(new.tenant_id);

    -- The actor is validated before the version management below: the
    -- validator accepts a username only while a current account row holds
    -- it, and a self write retires that row.
    new.modified_by := ores_iam_validate_account_username_fn(new.modified_by);

    -- Version management
    select version into current_version
    from "ores_refdata_book_product_types_tbl"
    where tenant_id = new.tenant_id
    and book_id = new.book_id
    and product_type_code = new.product_type_code
    and valid_to = ores_utility_infinity_timestamp_fn()
    for update;

    if found then
        if new.version != 0 and new.version != current_version then
            perform ores_outcome_raise_fn(
                'version_conflict',
                'book_product_types',
                'book_id',
                new.version::text,
                current_version::text);
        end if;
        new.version = current_version + 1;

        -- Close existing record.
        -- clock_timestamp(), not current_timestamp: current_timestamp is
        -- frozen for the whole transaction, so a same-transaction
        -- multi-write to this row (e.g. the same junction pair touched
        -- twice in one transaction) would collide with itself.
        -- clock_timestamp() always advances.
        update "ores_refdata_book_product_types_tbl"
        set valid_to = clock_timestamp()
        where tenant_id = new.tenant_id
        and book_id = new.book_id
        and product_type_code = new.product_type_code
        and valid_to = ores_utility_infinity_timestamp_fn()
        and valid_from < clock_timestamp();
    else
        new.version = 1;
    end if;

    new.valid_from = clock_timestamp();
    new.valid_to = ores_utility_infinity_timestamp_fn();

    new.performed_by = coalesce(ores_iam_current_service_fn(), current_user);

    new.change_reason_code := ores_dq_validate_change_reason_fn(new.tenant_id, new.change_reason_code);

    -- Validate book_id (soft FK to ores_refdata_books_tbl)
    if not exists (
        select 1 from ores_refdata_books_tbl
        where tenant_id = new.tenant_id
          and id = new.book_id
          and valid_to = ores_utility_infinity_timestamp_fn()
    ) then
        raise exception 'Invalid book_id: %. No active book found with this id.', new.book_id
            using errcode = '23503';
    end if;

    return new;
end;
$$ language plpgsql;

create or replace trigger ores_refdata_book_product_types_insert_trg
before insert on "ores_refdata_book_product_types_tbl"
for each row
execute function ores_refdata_book_product_types_insert_fn();

create or replace rule ores_refdata_book_product_types_delete_rule as
on delete to "ores_refdata_book_product_types_tbl"
do instead
  update "ores_refdata_book_product_types_tbl"
  set valid_to = clock_timestamp(),
      closed_by = coalesce(ores_iam_current_actor_fn(), current_user)
  where tenant_id = old.tenant_id
  and book_id = old.book_id
  and product_type_code = old.product_type_code
  and valid_to = ores_utility_infinity_timestamp_fn();

-- =============================================================================
-- Row-level security: tenant isolation for Book Product Type
-- =============================================================================
alter table "ores_refdata_book_product_types_tbl" enable row level security;

drop policy if exists book_product_types_tbl_tenant_isolation_policy
    on "ores_refdata_book_product_types_tbl";

create policy book_product_types_tbl_tenant_isolation_policy
on "ores_refdata_book_product_types_tbl"
for all using (
    tenant_id = ores_iam_current_tenant_id_fn()
)
with check (
    tenant_id = ores_iam_current_tenant_id_fn()
);
