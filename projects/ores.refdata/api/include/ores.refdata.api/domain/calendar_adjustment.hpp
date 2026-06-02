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
#ifndef ORES_REFDATA_API_DOMAIN_CALENDAR_ADJUSTMENT_HPP
#define ORES_REFDATA_API_DOMAIN_CALENDAR_ADJUSTMENT_HPP

#include <chrono>
#include <string>
#include <vector>
#include <optional>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::refdata::domain {

/**
 * @brief Overrides applied to a named ORE calendar.
 *
 * ORE ships built-in holiday calendars (identified by ISO currency code,
 * country code, or exchange MIC). A calendar adjustment patches one of those
 * built-in calendars with institution- or date-specific exceptions:
 * additional holidays (dates that would normally be business days) and
 * additional business days (dates that would normally be holidays).
 *
 * The @c calendar_name corresponds to the ORE @c name attribute on the
 * @c <Calendar> element (e.g. "DKK", "Japan", "CHF", "XNYS").
 *
 * Dates are stored as ISO-8601 strings ("YYYY-MM-DD") verbatim from the
 * source file to preserve exact round-trip fidelity.
 */
struct calendar_adjustment final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief ORE calendar name (e.g. "DKK", "Japan", "CHF", "XNYS").
     *
     * This is the natural key; the name matches the ORE @c name attribute and
     * maps to a QuantLib calendar identifier within the ORE engine.
     */
    std::string calendar_name;

    /**
     * @brief Optional base calendar this one derives from.
     *
     * When present, the adjusted calendar inherits all holidays from the
     * named base calendar before applying the additional overrides.
     */
    std::optional<std::string> base_calendar;

    /**
     * @brief Dates to treat as holidays in addition to the built-in rules.
     *
     * Each entry is an ISO-8601 date string ("YYYY-MM-DD").
     */
    std::vector<std::string> additional_holidays;

    /**
     * @brief Dates to treat as business days despite the built-in rules.
     *
     * Each entry is an ISO-8601 date string ("YYYY-MM-DD").
     */
    std::vector<std::string> additional_business_days;

    /**
     * @brief Username of the person who recorded this version in the system.
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
