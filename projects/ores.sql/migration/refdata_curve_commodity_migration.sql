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
 * One-shot migration: the commodity curve section
 *
 * Adds the tables that hold the commodity curve entries of a curve
 * configuration and their price segments, and lets a quote belong to a price
 * segment or to a named list such as a basis configuration's BasisQuotes.
 *
 * Run after refdata_curve_default_migration.sql. Every statement adds a column
 * if it is missing, or creates or replaces, so a database that already holds
 * these objects is left as it is.
 */

begin;

alter table ores_refdata_curve_quotes_tbl
    add column if not exists "commodity_price_segment_id" uuid,
    add column if not exists "quote_list" text;

\ir ../create/refdata/refdata_commodity_curves_create.sql
\ir ../create/refdata/refdata_commodity_curves_notify_trigger_create.sql
\ir ../create/refdata/refdata_commodity_price_segments_create.sql
\ir ../create/refdata/refdata_commodity_price_segments_notify_trigger_create.sql
\ir ../create/refdata/refdata_curve_quotes_create.sql
\ir ../create/refdata/refdata_rls_policies_create.sql

\ir ../populate/iam/iam_permissions_populate.sql

commit;
