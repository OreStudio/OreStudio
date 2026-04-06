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
#ifndef ORES_ANALYTICS_DOMAIN_PRICING_MODEL_CONFIG_HPP
#define ORES_ANALYTICS_DOMAIN_PRICING_MODEL_CONFIG_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::analytics::domain {

/**
 * @brief Named, versioned pricing model configuration header.
 *
 * A pricing model config is the top-level container for ORE's pricingengine.xml
 * content. It groups a set of per-product model/engine assignments and their
 * associated parameters under a single reusable, diffable name.
 *
 * config_variant distinguishes specialised ORE modes: "standard" for the
 * default engine, "amc" for American Monte Carlo, "dg" for dynamic grid, etc.
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
     * @brief UUID uniquely identifying this pricing model config.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique human-readable name for this configuration.
     *
     * Natural key within a tenant (e.g. "Base SIMM", "AMC Monte Carlo").
     */
    std::string name;

    /**
     * @brief Human-readable description of the configuration's purpose.
     */
    std::string description;

    /**
     * @brief ORE engine variant this config targets.
     *
     * Values: "standard", "amc", "dg", or empty for default.
     */
    std::optional<std::string> config_variant;

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
