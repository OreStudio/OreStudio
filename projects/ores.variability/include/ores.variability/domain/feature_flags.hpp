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
#ifndef ORES_VARIABILITY_DOMAIN_FEATURE_FLAGS_HPP
#define ORES_VARIABILITY_DOMAIN_FEATURE_FLAGS_HPP

#include <chrono>
#include <string>

namespace ores::variability::domain {

/**
 * @brief Represents a feature flag in the domain layer.
 */
struct feature_flags final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Flag indicating whether the feature is enabled or disabled.
     */
    bool enabled = false;

    /**
     * @brief Name of the feature flag, serves as the unique identifier.
     */
    std::string name;

    /**
     * @brief Description of what the feature flag controls.
     */
    std::string description;

    /**
     * @brief Username of the person who recorded this version in the system.
     */
    std::string recorded_by;

    /**
     * @brief Timestamp when this version of the record was recorded in the system.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
