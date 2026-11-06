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
#ifndef ORES_IAM_DOMAIN_ROLE_PRODUCT_TYPE_HPP
#define ORES_IAM_DOMAIN_ROLE_PRODUCT_TYPE_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::iam::domain {

/**
 * @brief A product type a role may book.
 *
 * +#+updated: 2026-11-03
 *
 * The product types a role may book.
 *
 * A permission says what a person may do; this says what they may do it to. The
 * verb is still gated by the trading permission, so a role that holds the booking
 * permission but no row here may book nothing, and a role with rows here and no
 * permission books nothing either: both halves are needed.
 *
 * A role with no rows permits no product type, which is the safe reading: the
 * junction is the grant, so a blank junction grants nothing. That is the opposite
 * of a book, where a blank set means everything, and the asymmetry is deliberate:
 * a book is a container that a tenant configures, and a role is an authority that
 * a tenant grants.
 *
 * The product type is named by the code the catalogue states, a soft reference
 * because the catalogue belongs to the trading component and a foreign key would
 * point the IAM tables at the trades.
 */
struct role_product_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief ID of the IAM role.
     *
     * References ores_iam_roles_tbl.id (soft FK).
     */
    boost::uuids::uuid role_id;

    /**
     * @brief Code of the product type the role permits.
     *
     * References ores_trading_product_types_tbl.code (soft FK, because the catalogue belongs to the
     * trading component).
     */
    std::string product_type_code;

    /**
     * @brief Username of the person who last modified this permitted product type.
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
    friend bool operator==(const role_product_type&, const role_product_type&) = default;
};

/**
 * @brief Dispatch-key identifier for role_product_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const role_product_type&) {
    return "ores.iam.role_product_type";
}

}

#endif
