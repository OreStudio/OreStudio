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
#ifndef ORES_REFDATA_API_DOMAIN_CALENDAR_EXCEPTION_HPP
#define ORES_REFDATA_API_DOMAIN_CALENDAR_EXCEPTION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief A one-off holiday/business-day override for a single calendar day.
 *
 * One row per one-off override for a single
 * [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][calendar]] day. Mirrors
 * ores::analytics::quant::domain::calendar_exception plus the usual
 * refdata plumbing. Applies uniformly to *any* calendar -- transcribed
 * or user-authored, based or base-less: a based calendar's full override
 * story is entirely calendar.base_calendar_code (which base) plus its
 * own exception rows (what's different), no separate "adjustment"
 * concept needed. QuantLib's own irregular, year-gated special dates
 * (the UK's Jubilee bank holidays, the NYSE's special closings) are
 * transcribed here, not folded into a
 * [[id:875E96F6-3FC7-4E0B-8E3C-2AC0F8BD488F][calendar_rule]] row, so a
 * rule stays a timeless, indefinitely-repeating pattern and the
 * genuinely irregular part of a calendar stays flat, no-logic-at-all
 * data.
 */
struct calendar_exception final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this exception row.
     *
     * Surrogate key -- a calendar can have many exceptions, so there is no single natural key
     * column.
     */
    boost::uuids::uuid id;

    /**
     * @brief The calendar this exception belongs to.
     *
     * References [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][ores_refdata_calendars_tbl.code]] (soft
     * FK).
     */
    std::string calendar_code;

    /**
     * @brief The specific calendar date this exception applies to.
     *
     * One exception per (calendar, date) pair.
     */
    std::chrono::year_month_day exception_date;

    /**
     * @brief Whether exception_date is overridden to a business day.
     *
     * true overrides a rule-generated holiday back to open; false adds an additional holiday not
     * covered by any rule.
     */
    bool is_business_day = false;

    /**
     * @brief Free-text note explaining the exception (e.g. "Queen's Platinum Jubilee", "Hurricane
     * Sandy").
     *
     * Purely informational; never interpreted by the instantiation engine.
     */
    std::optional<std::string> description;

    /**
     * @brief Username of the person who last modified this calendar exception.
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
 * @brief Dispatch-key identifier for calendar_exception, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const calendar_exception&) {
    return "ores.refdata.calendar_exception";
}

}

#endif
