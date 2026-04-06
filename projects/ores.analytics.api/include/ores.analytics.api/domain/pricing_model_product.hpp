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
#ifndef ORES_ANALYTICS_DOMAIN_PRICING_MODEL_PRODUCT_HPP
#define ORES_ANALYTICS_DOMAIN_PRICING_MODEL_PRODUCT_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::analytics::domain {

/**
 * @brief Per-product model and engine assignment within a pricing model config.
 *
 * Each row assigns a specific ORE model (e.g. "LGM", "DiscountedCashflows")
 * and engine (e.g. "Grid", "DiscountingSwapEngine") to a pricing engine type
 * within a named pricing model configuration. Corresponds to one <Product>
 * element inside a pricingengine.xml <PricingEngine> block.
 */
struct pricing_model_product final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this product assignment.
     */
    boost::uuids::uuid id;

    /**
     * @brief The pricing model config this product belongs to.
     *
     * Soft FK to ores_analytics_pricing_model_configs_tbl.
     */
    boost::uuids::uuid pricing_model_config_id;

    /**
     * @brief The ORE pricing engine type this assignment targets.
     *
     * Soft FK to ores_analytics_pricing_engine_types_tbl.
     * e.g. "EuropeanSwaption", "Swap", "CMS".
     */
    std::string pricing_engine_type_code;

    /**
     * @brief The ORE model to use for this product type.
     *
     * e.g. "LGM", "DiscountedCashflows", "BlackScholes".
     */
    std::string model;

    /**
     * @brief The ORE engine implementation to use.
     *
     * e.g. "Grid", "DiscountingSwapEngine", "AnalyticEuropeanEngine".
     */
    std::string engine;

    /**
     * @brief Username of the person who last modified this record.
     */
    std::string modified_by;

    /**
     * @brief Code identifying the reason for the change.
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

}

#endif
