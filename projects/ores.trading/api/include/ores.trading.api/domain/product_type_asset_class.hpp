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
#ifndef ORES_TRADING_DOMAIN_PRODUCT_TYPE_ASSET_CLASS_HPP
#define ORES_TRADING_DOMAIN_PRODUCT_TYPE_ASSET_CLASS_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Links a product type to a market it references.
 *
 * The markets a product type references. A kind may reference more than one: a
 * CrossCurrencySwap references interest rates and foreign exchange, an
 * InflationSwap references inflation and interest rates, a ConvertibleBond
 * references bonds and equity.
 *
 * One row per (product type, market) pair. Exactly one row per product type is
 * marked as the *home market*, the market the catalogue groups the kind under;
 * the rest are the markets it also references. A product type whose markets are
 * decided per trade, such as a container or a scripted trade, carries no rows at
 * all.
 *
 * The invariants are stated on the Product Type knowledge page.
 */
struct product_type_asset_class final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief The product type's code.
     *
     * References ores_trading_product_types_tbl.code (soft FK).
     */
    std::string product_type_code;

    /**
     * @brief The market's code, as the refdata asset class taxonomy states it.
     *
     * References ores_refdata_asset_class_codes_tbl.code (soft FK).
     */
    std::string asset_class_code;

    /**
     * @brief True on the one market the catalogue groups the product type under.
     */
    bool is_home;

    /**
     * @brief Username of the person who last modified this product type market.
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
    friend bool operator==(const product_type_asset_class&,
                           const product_type_asset_class&) = default;
};

/**
 * @brief Dispatch-key identifier for product_type_asset_class, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const product_type_asset_class&) {
    return "ores.trading.product_type_asset_class";
}

}

#endif
