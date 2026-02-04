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
#ifndef ORES_DQ_DOMAIN_DATASET_DEPENDENCY_HPP
#define ORES_DQ_DOMAIN_DATASET_DEPENDENCY_HPP

#include <chrono>
#include <optional>
#include <string>

namespace ores::dq::domain {

/**
 * @brief Declares a dependency between datasets using stable dataset codes.
 *
 * When a dataset is injected into the system, all its dependencies must be
 * satisfied first. Dependencies are resolved by code at injection time,
 * allowing for loose coupling between datasets.
 *
 * This enables building a dependency graph where, for example, an
 * "iso.countries" dataset can declare that it depends on "assets.country_flags"
 * for visual display.
 *
 * The dependency_code is intentionally NOT a foreign key - it's resolved at
 * injection time. This allows:
 * - Declaring dependencies on datasets that may not exist yet
 * - Referencing datasets that live in external systems
 * - Loose coupling with validation at injection time
 */
struct dataset_dependency final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief The code of the dataset that has the dependency.
     *
     * References dataset.code.
     */
    std::string dataset_code;

    /**
     * @brief The code of the dataset this depends on.
     *
     * This is NOT a foreign key - it's resolved at injection time.
     * Allows referencing datasets that may not exist yet or that
     * live in external systems.
     */
    std::string dependency_code;

    /**
     * @brief The role/purpose of this dependency.
     *
     * Examples: "visual_assets", "reference_data", "validation_source".
     */
    std::string role;

    /**
     * @brief Username of the person who recorded this dependency.
     */
    std::string recorded_by;

    /**
     * @brief Username of the account that performed this operation.
     */
    std::string performed_by;

    /**
     * @brief Timestamp when this dependency was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
