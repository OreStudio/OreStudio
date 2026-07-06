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
#ifndef ORES_REFDATA_API_DOMAIN_CURRENCY_PAIR_CLASSIFICATION_HPP
#define ORES_REFDATA_API_DOMAIN_CURRENCY_PAIR_CLASSIFICATION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>

namespace ores::refdata::domain {

/**
 * @brief Liquidity classification of a currency pair (major, minor, exotic).
 *
 * Reference data defining valid currency pair liquidity classifications.
 * Values include: major, minor, exotic — independent of either leg's own
 * market_tier (e.g. USD/NOK is major by liquidity even though NOK is
 * individually classified via the currency_group G11/Scandies system, not
 * this classification).
 */
struct currency_pair_classification final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique currency pair classification code.
     *
     * Examples: 'major', 'minor', 'exotic'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the classification.
     */
    std::string name;

    /**
     * @brief Detailed description of the classification.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this currency pair classification.
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
