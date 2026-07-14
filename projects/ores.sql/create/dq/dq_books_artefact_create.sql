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
create table if not exists "ores_dq_books_artefact_tbl" (
    "dataset_id" uuid not null,
    "tenant_id" uuid not null,
    "id" uuid not null,
    "version" integer not null,
    "name" text not null,
    "parent_portfolio_id" uuid not null,
    "functional_currency" text not null,
    "gl_account_ref" text null,
    "cost_center" text null,
    "book_status" text not null,
    "regulatory_book_type" text not null,
    "is_sweepable" boolean not null default false,
    -- Null means "region-agnostic -- inherit the publishing party's own
    -- business_center_code at publish time" (see
    -- ores_refdata_publish_books_from_dq_fn); a template can't know
    -- which party it will be published to. Only set this when the book
    -- itself is tied to a real trading desk location regardless of
    -- owning party (e.g. a GBP rates desk always trades out of London).
    "rates_centre_code" text null
);

create index if not exists books_artefact_dataset_idx
on "ores_dq_books_artefact_tbl" (dataset_id);

create index if not exists books_artefact_tenant_idx
on "ores_dq_books_artefact_tbl" (tenant_id);

create index if not exists books_artefact_id_idx
on "ores_dq_books_artefact_tbl" (id);

create index if not exists books_artefact_name_idx
on "ores_dq_books_artefact_tbl" (name);
