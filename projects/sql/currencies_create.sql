/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
create schema oresdb;
create extension btree_gist;
set schema 'oresdb';

create table currencies (
    name varchar(255),
    iso_code varchar(10),
    numeric_code int,
    symbol char(10),
    fraction_symbol char(10),
    fractions_per_unit int,
    rounding_type varchar(50),
    rounding_precision int,
    format varchar(50),
    currency_type varchar(20),
    modified_by varchar(255),
    valid_from timestamptz not null,
    valid_to timestamptz not null,
    primary key (iso_code, valid_from, valid_to),
    exclude using gist (
        iso_code WITH =,
        tstzrange(valid_from, valid_to) WITH &&
    )
);

create or replace function currencies_insert(
    name_p varchar(255),
    iso_code_p varchar(10),
    numeric_code_p int,
    symbol_p char(10),
    fraction_symbol_p char(10),
    fractions_per_unit_p int,
    rounding_type_p varchar(50),
    rounding_precision_p int,
    format_p varchar(50),
    currency_type_p varchar(20))
returns void as
$$
begin
    update oresdb.currencies
    set valid_to = current_timestamp
    where iso_code = iso_code_p and valid_to = 'infinity'::timestamp;

    insert into oresdb.currencies (
        name,
        iso_code,
        numeric_code,
        symbol,
        fraction_symbol,
        fractions_per_unit,
        rounding_type,
        rounding_precision,
        format,
        currency_type,
        modified_by,
        valid_from,
        valid_to)
    values (
        name_p,
        iso_code_p,
        numeric_code_p,
        symbol_p,
        fraction_symbol_p,
        fractions_per_unit_p,
        rounding_type_p,
        rounding_precision_p,
        format_p,
        currency_type_p,
        current_user,
        current_timestamp,
        'infinity'::timestamp);
end
$$
  language 'plpgsql';

create or replace view currencies_latest as
with latest_currencies as (
    select iso_code as lc_iso_code, max(valid_to) as max_valid_to
    from currencies
    group by iso_code)
select
    name,
    iso_code,
    numeric_code,
    symbol,
    fraction_symbol,
    fractions_per_unit,
    rounding_type,
    rounding_precision,
    format,
    currency_type,
    modified_by,
    valid_from,
    valid_to
from oresdb.currencies c
join latest_currencies lc
    on c.iso_code = lc.lc_iso_code and c.valid_to = lc.max_valid_to;

create or replace function currencies_as_of(as_of timestamptz)
returns table (
    name currencies.name%TYPE,
    iso_code currencies.iso_code%type,
    numeric_code currencies.numeric_code%type,
    symbol currencies.symbol%type,
    fraction_symbol currencies.fraction_symbol%type,
    fractions_per_unit currencies.fractions_per_unit%type,
    rounding_type currencies.rounding_type%type,
    rounding_precision currencies.rounding_precision%type,
    format currencies.format%type,
    currency_type currencies.currency_type%type,
    modified_by currencies.modified_by%type,
    valid_from currencies.valid_from%type,
    valid_to currencies.valid_to%type)
as $$
begin
    return query
    select
        c.name,
        c.iso_code,
        c.numeric_code,
        c.symbol,
        c.fraction_symbol,
        c.fractions_per_unit,
        c.rounding_type,
        c.rounding_precision,
        c.format,
        c.currency_type,
        c.modified_by,
        c.valid_from,
        c.valid_to
    from oresdb.currencies c
    where c.valid_from <= as_of and c.valid_to >= as_of;
end;
$$
language plpgsql
immutable;
