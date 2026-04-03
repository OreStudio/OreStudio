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
#ifndef ORES_REFDATA_API_DOMAIN_ZERO_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_ZERO_CONVENTION_HPP

#include <chrono>
#include <string>
#include <optional>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief Conventions for a zero-coupon yield curve.
 *
 * Specifies the day count fraction and compounding method used when
 * bootstrapping a discount or zero-rate curve in ORE. Corresponds to the
 * @c <Zero> element in @c conventions.xml.
 *
 * The @c id field is the natural key (ORE @c <Id> element).
 */
struct zero_convention final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique convention identifier (e.g. "EUR-ZERO-CONVENTIONS").
     */
    std::string id;

    /**
     * @brief Whether the curve is defined on a tenor grid (@c true) or on
     *        absolute dates (@c false).
     */
    bool tenor_based = false;

    /**
     * @brief Day count fraction code (canonical FpML, e.g. "ACT/365.FIXED").
     */
    std::string day_count_fraction;

    /**
     * @brief Compounding method (canonical CDM, e.g. "Continuous").
     * Present only when @c tenor_based is @c true.
     */
    std::optional<std::string> compounding;

    /**
     * @brief Frequency at which the rate is compounded (canonical CDM).
     * Present only when @c compounding is "Compounded".
     */
    std::optional<std::string> compounding_frequency;

    /**
     * @brief Calendar used to determine tenor dates (e.g. "TARGET").
     */
    std::optional<std::string> tenor_calendar;

    /**
     * @brief Number of business days from today to the spot date.
     */
    std::optional<int> spot_lag;

    /**
     * @brief Calendar used to compute the spot date.
     */
    std::optional<std::string> spot_calendar;

    /**
     * @brief Business day convention applied to tenor dates (canonical FpML).
     */
    std::optional<std::string> roll_convention;

    /**
     * @brief Whether end-of-month convention applies.
     */
    std::optional<bool> end_of_month;

    /**
     * @brief Username of the person who recorded this version.
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
