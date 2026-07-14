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
#ifndef ORES_REFDATA_API_DOMAIN_CALENDAR_HPP
#define ORES_REFDATA_API_DOMAIN_CALENDAR_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief ORE/QuantLib calendar reference data.
 *
 * Validated enumeration of named date collections consumed by ORE and
 * QuantLib: business-day/holiday calendars (TARGET, UnitedStates,
 * UnitedStates.GovernmentBond, ...), central-bank meeting calendars,
 * and other calendar-shaped reference data. Each row is one concrete
 * QuantLib/ORE calendar token — sub-market variants (e.g.
 * UnitedStates.NYSE vs UnitedStates.GovernmentBond) are separate
 * rows, not a joined variant field, so the code column always matches
 * ORE's XML <Calendar> vocabulary verbatim. Classified by
 * [[id:1A454661-81B5-4F8F-93A6-06547412DD84][calendar_type]] and
 * associated with the [[id:88E8E1FB-6F2F-495F-BEC4-8C7ABEF68563][country]]
 * whose calendar it is — supranational calendars (TARGET) use the
 * ZZ sentinel (ISO 3166-1's own reserved user-assigned code) rather
 * than a nullable country reference, since no single country owns them.
 */
struct calendar final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief QuantLib/ORE calendar token, verbatim (e.g. "TARGET", "UnitedStates",
     * "UnitedStates.GovernmentBond", "UnitedKingdom", "Japan").
     */
    std::string code;

    /**
     * @brief Human-readable, unique display name (e.g. "TARGET (Euro area)", "United States
     * (Government Bond)").
     */
    std::string name;

    /**
     * @brief Classification of the calendar's purpose; soft FK to ores_refdata_calendar_types_tbl
     * (e.g. "public_holiday", "central_bank_meeting", "financial_centre", "data_release", "other").
     */
    std::string calendar_type;

    /**
     * @brief ISO 3166-1 alpha-2 country code of the calendar's owning country; soft FK to
     * ores_refdata_countries_tbl. Supranational or non-country- specific calendars (e.g. "TARGET")
     * use the reserved ZZ user-assigned code rather than a nullable reference.
     */
    std::string country_code;

    /**
     * @brief Optional reference to a flag or logo image in the images table — overrides the
     * country-flag icon derived from country_code (e.g. for a central bank's own logo, or a
     * currency-union flag for a supranational calendar).
     */
    std::optional<boost::uuids::uuid> image_id;

    /**
     * @brief Username of the person who last modified this calendar.
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

/**
 * @brief Dispatch-key identifier for calendar, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const calendar&) {
    return "ores.refdata.calendar";
}

}

#endif
