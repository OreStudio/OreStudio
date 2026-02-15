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
#ifndef ORES_REFDATA_DOMAIN_BUSINESS_UNIT_HPP
#define ORES_REFDATA_DOMAIN_BUSINESS_UNIT_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::refdata::domain {

/**
 * @brief Internal organizational unit within a party.
 *
 * Represents internal organizational units (e.g., desks, departments, branches).
 * Supports hierarchical structure via self-referencing parent_business_unit_id.
 * Each unit belongs to a top-level legal entity (party).
 */
struct business_unit final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief UUID uniquely identifying this business unit.
     *
     * Surrogate key for the business unit record.
     */
    boost::uuids::uuid id;

    /**
     * @brief Top-level legal entity this unit belongs to.
     *
     * References the parent party record.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Human-readable name for the business unit.
     *
     * e.g., 'FX Options Desk', 'London Branch'.
     */
    std::string unit_name;

    /**
     * @brief Self-referencing parent unit for hierarchy.
     *
     * NULL indicates a top-level unit.
     */
    std::optional<boost::uuids::uuid> parent_business_unit_id;

    /**
     * @brief Optional internal code or alias.
     *
     * Short identifier for the business unit.
     */
    std::string unit_code;

    /**
     * @brief Business centre for the unit.
     *
     * References the business centres scheme. May be null for global units.
     */
    std::string business_centre_code;

    /**
     * @brief Username of the person who last modified this business unit.
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
