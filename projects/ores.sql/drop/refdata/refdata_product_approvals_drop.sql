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

-- The teardown of the product approval matrices: the triggers first, then the
-- functions they call. The tables the triggers hang from drop with their own
-- scripts.

drop trigger if exists ores_refdata_books_owner_unit_trg
    on "ores_refdata_books_tbl";
drop trigger if exists ores_refdata_business_units_approval_restriction_trg
    on "ores_refdata_business_units_tbl";
drop trigger if exists ores_refdata_business_unit_product_types_close_trg
    on "ores_refdata_business_unit_product_types_tbl";
drop trigger if exists ores_refdata_business_unit_product_types_approval_trg
    on "ores_refdata_business_unit_product_types_tbl";
drop trigger if exists ores_refdata_parties_approval_restriction_trg
    on "ores_refdata_parties_tbl";
drop trigger if exists ores_refdata_party_product_types_close_trg
    on "ores_refdata_party_product_types_tbl";
drop trigger if exists ores_refdata_book_product_types_approval_trg
    on "ores_refdata_book_product_types_tbl";

drop function if exists ores_refdata_books_owner_unit_fn();
drop function if exists ores_refdata_business_units_approval_restriction_fn();
drop function if exists ores_refdata_business_unit_product_types_close_fn();
drop function if exists ores_refdata_business_unit_product_types_approval_fn();
drop function if exists ores_refdata_business_unit_allowed_product_types_fn(uuid);
drop function if exists ores_refdata_parties_approval_restriction_fn();
drop function if exists ores_refdata_party_product_types_close_fn();
drop function if exists ores_refdata_book_product_types_approval_fn();
drop function if exists ores_refdata_book_allowed_product_types_fn(uuid);
drop function if exists ores_refdata_book_parent_allowed_product_types_fn(uuid, uuid);
drop function if exists ores_refdata_party_allowed_product_types_fn(uuid);
