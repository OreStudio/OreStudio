/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_DQ_DOMAIN_DATASET_BUNDLE_HPP
#define ORES_DQ_DOMAIN_DATASET_BUNDLE_HPP

#include <chrono>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::dq::domain {

/**
 * @brief A named collection of datasets designed to work together.
 *
 * Installing a bundle gets the system into a ready state with a coherent set
 * of reference data. Bundles provide a way to group related datasets that
 * should be installed together.
 *
 * Examples:
 * - "slovaris": Synthetic reference data for development and testing
 * - "base": Industry-standard reference data (ISO + FpML) for production
 * - "crypto": Base system plus cryptocurrency reference data
 */
struct dataset_bundle final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief UUID uniquely identifying this bundle.
     *
     * This is the surrogate key for the bundle.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique code for stable referencing.
     *
     * Examples: "slovaris", "base", "crypto".
     */
    std::string code;

    /**
     * @brief Human-readable name for the bundle.
     */
    std::string name;

    /**
     * @brief Detailed description of the bundle's contents and purpose.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this bundle.
     */
    std::string recorded_by;

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
