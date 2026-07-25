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
#ifndef ORES_REFDATA_DOMAIN_CALENDAR_DATE_HPP
#define ORES_REFDATA_DOMAIN_CALENDAR_DATE_HPP

#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One materialised (calendar, date) holiday/business-day fact, computed from a calendar
 * template's rules and exceptions.
 *
 * One row per (calendar_code, date), produced by the calendar
 * materialisation service instantiating a
 * [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][calendar]] template's
 * [[id:875E96F6-3FC7-4E0B-8E3C-2AC0F8BD488F][calendar_rule]] /
 * [[id:8B6E1C53-871E-4B6D-BD20-CB3F551B5C46][calendar_exception]] rows
 * over a rolling date range, never edited by hand. Read by every
 * consumer that needs "is date D a business day for calendar C" --
 * the UI (calendar detail screens, the holiday-aware date picker) and
 * ORE Studio's own tenor/schedule date-math -- so the answer is
 * consistent and never requires a live rule evaluation.
 */
struct calendar_date final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    std::string tenant_id;

    /**
     * @brief QuantLib/ORE calendar token (e.g. TARGET, UnitedStates).
     *
     * References ores_refdata_calendars_tbl.code (soft FK).
     */
    std::string calendar_code;

    /**
     * @brief The calendar date this row describes.
     */
    std::chrono::year_month_day date;

    /**
     * @brief Whether date is a business day for calendar_code -- false for both weekends and
     * holidays; the two are not distinguished here. A reader that needs to shade "weekend vs.
     * holiday" differently gets that from re-deriving the weekend mask itself (see
     * ores.analytics.quant's business_day_calendar_set), not from this column.
     */
    bool is_business_day;

    /**
     * @brief How this row was instantiated. Soft-enum: quantlib_computed (base-less
     * source'quantlib'= calendar, instantiated by ores.analytics.quant's batch engine) |
     * user_adjustment (base-having source'user'= calendar: base template's materialised dates
     * overlaid with this calendar's own
     * [[id:8B6E1C53-871E-4B6D-BD20-CB3F551B5C46][calendar_exception]] rows) | user_defined
     * (base-less source'user'= calendar: its calendar_exception rows copied in directly, no
     * computation). Always shown next to any displayed date -- the origin must never be implicit.
     */
    std::string source;

    /**
     * @brief Username of the person who last modified this calendar date.
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
 * @brief Dispatch-key identifier for calendar_date, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const calendar_date&) {
    return "ores.refdata.calendar_date";
}

}

#endif
