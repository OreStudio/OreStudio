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
     * @brief Soft-enum: quantlib | user | federalreserve.gov. quantlib means this template's rule
     * set was transcribed from QuantLib's published rules and matches one of ORE's recognised
     * built-in calendar names -- a taxonomic fact driving read-only enforcement and the ORE-export
     * skip logic, *not* an instruction to call the QuantLib library (QuantLib is not linked at
     * runtime; see this task's * Revision section). user means a calendar template authored in ORE
     * Studio, fully editable. federalreserve.gov marks the ORE-native US.FOMC calendar, whose
     * meeting dates are transcribed from the Fed's published calendar into calendar_event rows
     * (story: FOMC-dated OIS short end); it is editable like user rows but is not a general
     * user-authored template.
     */
    std::string source;

    /**
     * @brief Whether users can edit this template's own row (name, calendar_type, country_code,
     * ...) directly. Independent column from source -- every quantlib row is inserted with false
     * and every user row with true, but that pairing is enforced by the insert trigger's
     * validation, not hardcoded from source in application code, per this task's instruction that
     * read-only/editable be tracked per-template rather than assumed from source.
     */
    bool is_editable = false;

    /**
     * @brief Soft FK to this same table's own code column (self-referential). Only meaningful for
     * source  'user'= rows: present means this template is a delta on top of another template
     * (QuantLib-sourced or itself user-authored) via that template's own
     * [[id:8B6E1C53-871E-4B6D-BD20-CB3F551B5C46][calendar_exception]] rows; absent means a wholly
     * bespoke calendar with no base. Always null for source  'quantlib'= rows.
     */
    std::optional<std::string> base_calendar_code;

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
