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
#ifndef ORES_ANALYTICS_API_DOMAIN_PRICING_ENGINE_TYPE_HPP
#define ORES_ANALYTICS_API_DOMAIN_PRICING_ENGINE_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief Pricing engine type code identifying the product granularity used by the pricing engine.
 *
 * Classification of products at the granularity needed by the pricing engine to select the correct
 * model and numerical method (e.g. EuropeanSwaption, BermudanSwaption, CMS).
 */
struct pricing_engine_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique pricing engine type code.
     *
     * Examples: 'Swap', 'EuropeanSwaption', 'BermudanSwaption', 'CMS', 'CapFloor'.
     */
    std::string code;

    /**
     * @brief Human-readable description of the pricing engine type.
     */
    std::string description;

    /**
     * @brief Reference to the corresponding instrument type code. Use the NONE sentinel for engine
     * types (e.g. leg-/coupon-level pricers) with no direct instrument type mapping.
     *
     * Soft FK to ores_refdata_instrument_codes_tbl.
     */
    std::string instrument_type_code;

    /**
     * @brief Username of the person who last modified this pricing engine type.
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
};

/**
 * @brief Dispatch-key identifier for pricing_engine_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const pricing_engine_type&) {
    return "ores.analytics.pricing_engine_type";
}

}

#endif
