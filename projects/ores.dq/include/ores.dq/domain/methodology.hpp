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
#ifndef ORES_DQ_DOMAIN_METHODOLOGY_HPP
#define ORES_DQ_DOMAIN_METHODOLOGY_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::dq::domain {

/**
 * @brief Describes a methodology for data processing or transformation.
 */
struct methodology final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief UUID uniquely identifying this methodology.
     *
     * This is the surrogate key for the methodology.
     */
    boost::uuids::uuid id;

    /**
     * @brief Human-readable name for the methodology.
     */
    std::string name;

    /**
     * @brief Detailed description of the methodology's purpose and approach.
     */
    std::string description;

    /**
     * @brief Optional reference to external documentation of the methodology logic.
     *
     * Typically a URL or document reference.
     */
    std::optional<std::string> logic_reference;

    /**
     * @brief Optional details about how the methodology is implemented.
     */
    std::optional<std::string> implementation_details;

    /**
     * @brief Username of the person who last modified this methodology.
     */
    std::string recorded_by;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Username of the account that performed this operation.
     */
    std::string performed_by;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
