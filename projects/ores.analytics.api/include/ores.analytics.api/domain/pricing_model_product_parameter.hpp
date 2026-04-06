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
#ifndef ORES_ANALYTICS_DOMAIN_PRICING_MODEL_PRODUCT_PARAMETER_HPP
#define ORES_ANALYTICS_DOMAIN_PRICING_MODEL_PRODUCT_PARAMETER_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::analytics::domain {

/**
 * @brief Normalised parameter row for a pricing model configuration.
 *
 * Stores one name/value pair for either a product-scoped parameter (model or
 * engine parameter for a specific product) or a global config-scoped parameter
 * that applies across all products. Normalised rows enable granular diffing for
 * P&L attribution and change tracking.
 *
 * parameter_scope discriminates between:
 *   "model"  — parameter passed to the model (pricing_model_product_id set)
 *   "engine" — parameter passed to the engine (pricing_model_product_id set)
 *   "global" — config-level parameter (pricing_model_product_id is null)
 */
struct pricing_model_product_parameter final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this parameter row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The pricing model config this parameter belongs to.
     *
     * Soft FK to ores_analytics_pricing_model_configs_tbl. Always set.
     */
    boost::uuids::uuid pricing_model_config_id;

    /**
     * @brief The product assignment this parameter applies to.
     *
     * Soft FK to ores_analytics_pricing_model_products_tbl.
     * Null for global (config-scoped) parameters.
     */
    std::optional<boost::uuids::uuid> pricing_model_product_id;

    /**
     * @brief Scope of this parameter.
     *
     * One of: "model", "engine", "global".
     */
    std::string parameter_scope;

    /**
     * @brief Parameter name (e.g. "Calibration", "sy", "Tolerance").
     */
    std::string parameter_name;

    /**
     * @brief Parameter value, always stored as text.
     */
    std::string parameter_value;

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
