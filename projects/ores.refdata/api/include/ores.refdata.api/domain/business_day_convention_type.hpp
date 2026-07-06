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
#ifndef ORES_REFDATA_API_DOMAIN_BUSINESS_DAY_CONVENTION_TYPE_HPP
#define ORES_REFDATA_API_DOMAIN_BUSINESS_DAY_CONVENTION_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>

namespace ores::refdata::domain {

/**
 * @brief ORE business day convention code (e.g. Following, ModifiedFollowing, Unadjusted).
 *
 * Reference data table defining valid business day conventions used to
 * adjust dates that fall on a non-business day (e.g. for coupon/settlement
 * dates on instrument legs and currency pair conventions). Values are
 * sourced from ORE's ore_types.xsd. Moved here from ores.trading
 * (see [[id:0345DCE3-4B85-4132-9A25-E58285632F76][Commission: business_day_convention_type]]) since
 * every consumer of this type — the *_convention entities and now
 * [[id:109FBFD6-C505-4196-B3EE-D5C798CE050F][fx_convention]] — lives in ores.refdata, not
 * ores.trading.
 */
struct business_day_convention_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique business day convention type code.
     *
     * Examples: 'Following', 'ModifiedFollowing', 'Preceding', 'Unadjusted'.
     */
    std::string code;

    /**
     * @brief Human-readable name of the business day convention (e.g. "Modified Following").
     */
    std::string name;

    /**
     * @brief Human-readable description of the business day convention.
     */
    std::string description;

    /**
     * @brief Ordinal position for dropdown/list display.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this business day convention type.
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
