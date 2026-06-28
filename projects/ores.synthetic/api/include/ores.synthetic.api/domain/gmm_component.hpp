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
#ifndef ORES_SYNTHETIC_DOMAIN_GMM_COMPONENT_HPP
#define ORES_SYNTHETIC_DOMAIN_GMM_COMPONENT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>

namespace ores::synthetic::domain {

/**
 * @brief A single component of a Gaussian Mixture Model price process.
 *
 * An fx_spot_generation_config drives its synthetic price increments from a
 * Gaussian Mixture Model: a weighted set of normal distributions. Each
 * component is one normal distribution (mean, standard deviation) with a
 * mixture weight; together the components define the increment distribution
 * sampled per tick.
 *
 * Scoped to a tenant and a party so each party manages its own configurations
 * and its own generated data.
 */
struct gmm_component final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique identifier for this component.
     */
    boost::uuids::uuid id{};

    /**
     * @brief Owning party; the component and the data it helps generate belong
     * to this party within the tenant.
     */
    boost::uuids::uuid party_id{};

    /**
     * @brief Owning FX spot generation config.
     *
     * References the fx_spot_generation_configs table (soft FK).
     */
    boost::uuids::uuid fx_spot_config_id{};

    /**
     * @brief Position of this component within its mixture (0-based ordering).
     */
    int component_index = 0;

    /**
     * @brief Mean of this component's normal distribution.
     */
    double mean = 0.0;

    /**
     * @brief Standard deviation of this component's normal distribution.
     */
    double stdev = 0.0;

    /**
     * @brief Mixture weight of this component (relative probability).
     */
    double weight = 0.0;

    /**
     * @brief Username of the person who recorded this version in the system.
     */
    std::string modified_by;

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
     * @brief Username of the account that performed this operation.
     */
    std::string performed_by;

    /**
     * @brief Timestamp when this version of the record was recorded in the system.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
