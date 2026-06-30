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
#ifndef ORES_SYNTHETIC_API_DOMAIN_GMM_COMPONENT_HPP
#define ORES_SYNTHETIC_API_DOMAIN_GMM_COMPONENT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>

namespace ores::synthetic::domain {

/**
 * @brief One weighted Gaussian component of an FX spot GMM price process.
 *
 * A single weighted Gaussian component (mean, standard deviation, weight) of the
 * Gaussian Mixture Model that drives an fx_spot_generation_config's price
 * process. Components belong to a parent FX spot config via fx_spot_config_id;
 * their weights are normalised at generation time. Party- and tenant-scoped.
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
     * @brief Surrogate UUID uniquely identifying this GMM component.
     */
    boost::uuids::uuid id;

    /**
     * @brief Owning party (legal entity) this component's configuration belongs to.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Parent fx_spot_generation_config this component contributes to.
     */
    boost::uuids::uuid fx_spot_config_id;

    /**
     * @brief Zero-based ordinal of this component within its parent's mixture.
     */
    int component_index;

    /**
     * @brief Zero-based ordinal of this component within its parent's mixture.
     */
    int component_index = 0;

    /**
     * @brief Human-readable label for the component (e.g., "primary", "jump").
     */
    std::string description;

    /**
     * @brief Mean log-return (drift) of this Gaussian component per update.
     */
    double mean = 0.0;

    /**
     * @brief Standard deviation (volatility) of this Gaussian component; must be >= 0.
     */
    double stdev = 0.0;

    /**
     * @brief Mixture weight of this component; normalised across the parent's set.
     */
    double weight = 0.0;

    /**
     * @brief Username of the person who last modified this GMM component.
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

}

#endif
