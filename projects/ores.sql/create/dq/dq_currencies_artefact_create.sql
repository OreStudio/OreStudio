/* -*- sql-product: postgres; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
set schema 'metadata';

create table if not exists "metadata"."dq_currencies_artefact_tbl" (
    "dataset_id" uuid not null,
    "iso_code" text not null,
    "version" integer not null,
    "name" text not null,
    "numeric_code" text not null,
    "symbol" text not null,
    "fraction_symbol" text not null,
    "fractions_per_unit" integer not null,
    "rounding_type" text not null,
    "rounding_precision" integer not null,
    "format" text not null,
    "currency_type" text not null,
    "image_id" uuid
);

create index if not exists dq_currencies_artefact_dataset_idx
on "metadata"."dq_currencies_artefact_tbl" (dataset_id);

create index if not exists dq_currencies_artefact_iso_code_idx
on "metadata"."dq_currencies_artefact_tbl" (iso_code);

create index if not exists dq_currencies_artefact_numeric_code_idx
on "metadata"."dq_currencies_artefact_tbl" (numeric_code);

create index if not exists dq_currencies_artefact_currency_type_idx
on "metadata"."dq_currencies_artefact_tbl" (currency_type);
