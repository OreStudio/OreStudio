/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REFDATA_DOMAIN_BOOK_PRODUCT_TYPE_HPP
#define ORES_REFDATA_DOMAIN_BOOK_PRODUCT_TYPE_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief A product type a book allows, when the book states its allowed set.
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
struct book_product_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief ID of the book, or 00000000-0000-0000-0000-000000000000 for a book that does not
     * exist.
     *
     * References ores_refdata_books_tbl.id (soft FK).
     */
    boost::uuids::uuid book_id;

    /**
     * @brief Code of the product type the book allows.
     *
     * References ores_trading_product_types_tbl.code (soft FK, because the catalogue belongs to the
     * trading component).
     */
    std::string product_type_code;

    /**
     * @brief Username of the person who last modified this allowed product type.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality, on the same terms as an entity's.
     */
    friend bool operator==(const book_product_type&, const book_product_type&) = default;
};

/**
 * @brief Dispatch-key identifier for book_product_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const book_product_type&) {
    return "ores.refdata.book_product_type";
}

}

#endif
