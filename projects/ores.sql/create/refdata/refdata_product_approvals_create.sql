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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

-- =============================================================================
-- The product approval matrices, and the rule that joins them
-- =============================================================================
-- A party states the product types it permits, and each of its books may state
-- a subset of them. Both sets are junctions with a flag on the row they hang
-- from, and the absence of rows means opposite things on either side of the
-- flag: every type when the flag is false, and none when it is true.
--
-- The functions below resolve a set from the two matrices without reading the
-- product type catalogue, which belongs to the trading component. A null answer
-- therefore means "every type the catalogue states", and it is the caller that
-- knows the catalogue. An empty array means none.
--
-- The triggers keep the subset rule true on both sides of it: a book may not
-- name a type its party does not permit, a party may not drop a type a book
-- still carries, and a party may not start restricting while a book carries a
-- type outside the set it is about to state.

create or replace function ores_refdata_party_allowed_product_types_fn(
    p_party_id uuid
)
returns text[] as $$
declare
    v_restricts boolean;
begin
    select p.restricts_product_types into v_restricts
    from ores_refdata_parties_tbl p
    where p.id = p_party_id
      and p.valid_to = ores_utility_infinity_timestamp_fn();

    -- A party the caller cannot see answers no set rather than an open one, so
    -- a check that reads the answer refuses instead of permitting.
    if v_restricts is null then
        return '{}'::text[];
    end if;

    if not v_restricts then
        return null;
    end if;

    return coalesce(
        (select array_agg(distinct j.product_type_code order by j.product_type_code)
         from ores_refdata_party_product_types_tbl j
         where j.party_id = p_party_id
           and j.valid_to = ores_utility_infinity_timestamp_fn()),
        '{}'::text[]);
end;
$$ language plpgsql stable;

comment on function ores_refdata_party_allowed_product_types_fn(uuid) is
    'The product types a party permits, or null when it permits every type the catalogue states.';

create or replace function ores_refdata_business_unit_allowed_product_types_fn(
    p_business_unit_id uuid
)
returns text[] as $$
declare
    v_restricts boolean;
    v_party_id uuid;
begin
    select u.restricts_product_types, u.party_id into v_restricts, v_party_id
    from ores_refdata_business_units_tbl u
    where u.id = p_business_unit_id
      and u.valid_to = ores_utility_infinity_timestamp_fn();

    if v_restricts is null then
        return '{}'::text[];
    end if;

    if not v_restricts then
        return ores_refdata_party_allowed_product_types_fn(v_party_id);
    end if;

    return coalesce(
        (select array_agg(distinct j.product_type_code order by j.product_type_code)
         from ores_refdata_business_unit_product_types_tbl j
         where j.business_unit_id = p_business_unit_id
           and j.valid_to = ores_utility_infinity_timestamp_fn()),
        '{}'::text[]);
end;
$$ language plpgsql stable;

comment on function ores_refdata_business_unit_allowed_product_types_fn(uuid) is
    'The product types a desk permits, resolved through its party, or null when both permit every type the catalogue states.';

create or replace function ores_refdata_book_allowed_product_types_fn(
    p_book_id uuid
) returns text[] as $$
declare
    v_restricts boolean;
    v_party_id uuid;
    v_business_unit_id uuid;
begin
    select b.restricts_product_types, b.party_id, b.owner_unit_id
    into v_restricts, v_party_id, v_business_unit_id
    from ores_refdata_books_tbl b
    where b.id = p_book_id
      and b.valid_to = ores_utility_infinity_timestamp_fn();

    if v_restricts is null then
        return '{}'::text[];
    end if;

    -- A book that states no set carries its parent's, and the parent is its desk
    -- where it names one and its party where it does not.
    if not v_restricts then
        return case
                   when v_business_unit_id is null
                       then ores_refdata_party_allowed_product_types_fn(v_party_id)
                   else ores_refdata_business_unit_allowed_product_types_fn(v_business_unit_id)
               end;
    end if;

    return coalesce(
        (select array_agg(distinct j.product_type_code order by j.product_type_code)
         from ores_refdata_book_product_types_tbl j
         where j.book_id = p_book_id
           and j.valid_to = ores_utility_infinity_timestamp_fn()),
        '{}'::text[]);
end;
$$ language plpgsql stable;

comment on function ores_refdata_book_allowed_product_types_fn(uuid) is
    'The product types a book may carry, resolved from the book and its party, or null when both permit every type the catalogue states.';

-- A book may name a type only when its party permits it.
create or replace function ores_refdata_book_product_types_approval_fn()
returns trigger as $$
declare
    v_party_id uuid;
    v_business_unit_id uuid;
    v_allowed text[];
begin
    if new.valid_to <> ores_utility_infinity_timestamp_fn() then
        return new;
    end if;

    select b.party_id, b.owner_unit_id into v_party_id, v_business_unit_id
    from ores_refdata_books_tbl b
    where b.id = new.book_id
      and b.valid_to = ores_utility_infinity_timestamp_fn();

    if v_party_id is null then
        return new;
    end if;

    v_allowed := case
                     when v_business_unit_id is null
                         then ores_refdata_party_allowed_product_types_fn(v_party_id)
                     else ores_refdata_business_unit_allowed_product_types_fn(v_business_unit_id)
                 end;
    if v_allowed is null then
        return new;
    end if;

    if not (new.product_type_code = any(v_allowed)) then
        raise exception
            'Book % may not carry product type %: its party permits %.',
            new.book_id, new.product_type_code,
            coalesce(nullif(array_to_string(v_allowed, ', '), ''), 'nothing')
            using errcode = '23514';
    end if;

    return new;
end;
$$ language plpgsql;

comment on function ores_refdata_book_product_types_approval_fn() is
    'Refuses a book product type its party does not permit.';

-- A party may not drop a type a book of its own still carries.
create or replace function ores_refdata_party_product_types_close_fn()
returns trigger as $$
begin
    if old.valid_to = ores_utility_infinity_timestamp_fn()
       and new.valid_to <> ores_utility_infinity_timestamp_fn() then
        perform 1
        from ores_refdata_business_units_tbl u
        join ores_refdata_business_unit_product_types_tbl j
          on j.business_unit_id = u.id
         and j.product_type_code = old.product_type_code
         and j.valid_to = ores_utility_infinity_timestamp_fn()
        where u.party_id = old.party_id
          and u.restricts_product_types
          and u.valid_to = ores_utility_infinity_timestamp_fn();
        if found then
            raise exception
                'Party % may not drop product type %: a desk of the party permits it.',
                old.party_id, old.product_type_code
                using errcode = '23514';
        end if;

        perform 1
        from ores_refdata_books_tbl b
        join ores_refdata_book_product_types_tbl j
          on j.book_id = b.id
         and j.product_type_code = old.product_type_code
         and j.valid_to = ores_utility_infinity_timestamp_fn()
        where b.party_id = old.party_id
          and b.restricts_product_types
          and b.valid_to = ores_utility_infinity_timestamp_fn();
        if found then
            raise exception
                'Party % may not drop product type %: a book of the party carries it.',
                old.party_id, old.product_type_code
                using errcode = '23514';
        end if;
    end if;

    return new;
end;
$$ language plpgsql;

comment on function ores_refdata_party_product_types_close_fn() is
    'Refuses a party product type that a book of the party still carries.';

-- A party may not start restricting while a book carries a type outside the
-- set the party is about to state.
create or replace function ores_refdata_parties_approval_restriction_fn()
returns trigger as $$
declare
    v_outside text;
begin
    if not new.restricts_product_types or old.restricts_product_types then
        return new;
    end if;

    select j.product_type_code into v_outside
    from ores_refdata_business_units_tbl u
    join ores_refdata_business_unit_product_types_tbl j
      on j.business_unit_id = u.id
     and j.valid_to = ores_utility_infinity_timestamp_fn()
    where u.party_id = new.id
      and u.restricts_product_types
      and u.valid_to = ores_utility_infinity_timestamp_fn()
      and not exists (
          select 1
          from ores_refdata_party_product_types_tbl p
          where p.party_id = new.id
            and p.product_type_code = j.product_type_code
            and p.valid_to = ores_utility_infinity_timestamp_fn())
    limit 1;

    if v_outside is not null then
        raise exception
            'Party % may not start restricting: a desk permits product type %, which the party would not permit.',
            new.id, v_outside
            using errcode = '23514';
    end if;

    select j.product_type_code into v_outside
    from ores_refdata_books_tbl b
    join ores_refdata_book_product_types_tbl j
      on j.book_id = b.id
     and j.valid_to = ores_utility_infinity_timestamp_fn()
    where b.party_id = new.id
      and b.restricts_product_types
      and b.valid_to = ores_utility_infinity_timestamp_fn()
      and not exists (
          select 1
          from ores_refdata_party_product_types_tbl p
          where p.party_id = new.id
            and p.product_type_code = j.product_type_code
            and p.valid_to = ores_utility_infinity_timestamp_fn())
    limit 1;

    if v_outside is not null then
        raise exception
            'Party % may not start restricting: a book carries product type %, which the party would not permit.',
            new.id, v_outside
            using errcode = '23514';
    end if;

    return new;
end;
$$ language plpgsql;

comment on function ores_refdata_parties_approval_restriction_fn() is
    'Refuses a party that starts restricting while a book carries a type the party would not permit.';

-- A desk may name a type only when its party permits it.
create or replace function ores_refdata_business_unit_product_types_approval_fn()
returns trigger as $$
declare
    v_party_id uuid;
    v_allowed text[];
begin
    if new.valid_to <> ores_utility_infinity_timestamp_fn() then
        return new;
    end if;

    select u.party_id into v_party_id
    from ores_refdata_business_units_tbl u
    where u.id = new.business_unit_id
      and u.valid_to = ores_utility_infinity_timestamp_fn();

    if v_party_id is null then
        return new;
    end if;

    v_allowed := ores_refdata_party_allowed_product_types_fn(v_party_id);
    if v_allowed is null then
        return new;
    end if;

    if not (new.product_type_code = any(v_allowed)) then
        raise exception
            'Business unit % may not permit product type %: its party permits %.',
            new.business_unit_id, new.product_type_code,
            coalesce(nullif(array_to_string(v_allowed, ', '), ''), 'nothing')
            using errcode = '23514';
    end if;

    return new;
end;
$$ language plpgsql;

comment on function ores_refdata_business_unit_product_types_approval_fn() is
    'Refuses a desk product type its party does not permit.';

-- A desk may not drop a type a book of its own still carries.
create or replace function ores_refdata_business_unit_product_types_close_fn()
returns trigger as $$
begin
    if old.valid_to = ores_utility_infinity_timestamp_fn()
       and new.valid_to <> ores_utility_infinity_timestamp_fn() then
        perform 1
        from ores_refdata_books_tbl b
        join ores_refdata_book_product_types_tbl j
          on j.book_id = b.id
         and j.product_type_code = old.product_type_code
         and j.valid_to = ores_utility_infinity_timestamp_fn()
        where b.owner_unit_id = old.business_unit_id
          and b.restricts_product_types
          and b.valid_to = ores_utility_infinity_timestamp_fn();
        if found then
            raise exception
                'Business unit % may not drop product type %: a book of the desk carries it.',
                old.business_unit_id, old.product_type_code
                using errcode = '23514';
        end if;
    end if;

    return new;
end;
$$ language plpgsql;

comment on function ores_refdata_business_unit_product_types_close_fn() is
    'Refuses a desk product type that a book of the desk still carries.';

-- A desk may not start restricting while a book carries a type outside the set
-- the desk is about to state.
create or replace function ores_refdata_business_units_approval_restriction_fn()
returns trigger as $$
declare
    v_outside text;
begin
    if not new.restricts_product_types or old.restricts_product_types then
        return new;
    end if;

    select j.product_type_code into v_outside
    from ores_refdata_books_tbl b
    join ores_refdata_book_product_types_tbl j
      on j.book_id = b.id
     and j.valid_to = ores_utility_infinity_timestamp_fn()
    where b.owner_unit_id = new.id
      and b.restricts_product_types
      and b.valid_to = ores_utility_infinity_timestamp_fn()
      and not exists (
          select 1
          from ores_refdata_business_unit_product_types_tbl p
          where p.business_unit_id = new.id
            and p.product_type_code = j.product_type_code
            and p.valid_to = ores_utility_infinity_timestamp_fn())
    limit 1;

    if v_outside is not null then
        raise exception
            'Business unit % may not start restricting: a book carries product type %, which the desk would not permit.',
            new.id, v_outside
            using errcode = '23514';
    end if;

    return new;
end;
$$ language plpgsql;

comment on function ores_refdata_business_units_approval_restriction_fn() is
    'Refuses a desk that starts restricting while a book carries a type the desk would not permit.';

drop trigger if exists ores_refdata_book_product_types_approval_trg
    on "ores_refdata_book_product_types_tbl";
create trigger ores_refdata_book_product_types_approval_trg
before insert or update on "ores_refdata_book_product_types_tbl"
for each row execute function ores_refdata_book_product_types_approval_fn();

drop trigger if exists ores_refdata_party_product_types_close_trg
    on "ores_refdata_party_product_types_tbl";
create trigger ores_refdata_party_product_types_close_trg
before update on "ores_refdata_party_product_types_tbl"
for each row execute function ores_refdata_party_product_types_close_fn();

drop trigger if exists ores_refdata_parties_approval_restriction_trg
    on "ores_refdata_parties_tbl";
create trigger ores_refdata_parties_approval_restriction_trg
before update on "ores_refdata_parties_tbl"
for each row execute function ores_refdata_parties_approval_restriction_fn();

drop trigger if exists ores_refdata_business_unit_product_types_approval_trg
    on "ores_refdata_business_unit_product_types_tbl";
create trigger ores_refdata_business_unit_product_types_approval_trg
before insert or update on "ores_refdata_business_unit_product_types_tbl"
for each row execute function ores_refdata_business_unit_product_types_approval_fn();

drop trigger if exists ores_refdata_business_unit_product_types_close_trg
    on "ores_refdata_business_unit_product_types_tbl";
create trigger ores_refdata_business_unit_product_types_close_trg
before update on "ores_refdata_business_unit_product_types_tbl"
for each row execute function ores_refdata_business_unit_product_types_close_fn();

drop trigger if exists ores_refdata_business_units_approval_restriction_trg
    on "ores_refdata_business_units_tbl";
create trigger ores_refdata_business_units_approval_restriction_trg
before update on "ores_refdata_business_units_tbl"
for each row execute function ores_refdata_business_units_approval_restriction_fn();
