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
#ifndef ORES_ANALYTICS_API_DOMAIN_PRICING_MODEL_CONFIG_HPP
#define ORES_ANALYTICS_API_DOMAIN_PRICING_MODEL_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief Named pricing model configuration mapping pricing engine types to models and engines.
 *
 * Header entity for a pricing model configuration. Each config contains product mappings
 * (in pricing_model_products) and parameters (in pricing_model_product_parameters).
 * Corresponds to ORE's pricingengine.xml.
 */
struct pricing_model_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this pricing model configuration.
     *
     * Surrogate key for the pricing model configuration.
     */
    boost::uuids::uuid id;

    /**
     * @brief Human-readable name for this configuration.
     *
     * Unique per tenant. Examples: 'Standard', 'AMC', 'DeltaGamma', 'SABR'.
     */
    std::string name;

    /**
     * @brief Detailed description of this pricing model configuration.
     */
    std::string description;

    /**
     * @brief Configuration variant tag.
     *
     * Examples: 'standard', 'amc', 'amccg', 'dg', 'sabr', 'ad'.
     */
    std::string config_variant;

    /**
     * @brief Username of the person who last modified this pricing model configuration.
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
 * @brief Dispatch-key identifier for pricing_model_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const pricing_model_config&) {
    return "ores.analytics.pricing_model_config";
}

}

#endif
