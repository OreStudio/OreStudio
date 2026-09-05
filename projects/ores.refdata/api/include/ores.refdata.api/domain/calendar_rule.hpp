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
#ifndef ORES_REFDATA_API_DOMAIN_CALENDAR_RULE_HPP
#define ORES_REFDATA_API_DOMAIN_CALENDAR_RULE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief An indefinite recurring rule for a base-less calendar template.
 *
 * One row per indefinite, timeless recurring holiday rule for a
 * base-less [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][calendar]]
 * template -- the QuantLib-transcribed calendars and any base-less
 * user-authored template alike. Mirrors
 * ores::analytics::quant::domain::calendar_rule plus the usual
 * refdata plumbing; a calendar's full rule set is the batch input to
 * that pure engine at materialisation time (see the parent task's
 * * Revision section for the two-stage rule/materialisation design).
 * One-off irregular dates (jubilees, special closings) are *not*
 * modelled here -- they belong in
 * [[id:8B6E1C53-871E-4B6D-BD20-CB3F551B5C46][calendar_exception]] instead, so a rule
 * stays a timeless, indefinitely-repeating pattern.
 */
struct calendar_rule final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this rule row.
     *
     * Surrogate key -- a calendar can have many rules, so there is no single natural key column.
     */
    boost::uuids::uuid id;

    /**
     * @brief The calendar this rule belongs to.
     *
     * References [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][ores_refdata_calendars_tbl.code]] (soft
     * FK) -- only meaningful for base-less calendars (calendar.base_calendar_code is null).
     */
    std::string calendar_code;

    /**
     * @brief Which of the four rule shapes this row is.
     *
     * Soft-enum: fixed_date | nth_weekday_of_month | last_weekday_of_month | easter_offset,
     * matching ores::analytics::quant::domain::calendar_rule_kind exactly, mapped 1:1 at the
     * refdata/quant boundary.
     */
    std::string kind;

    /**
     * @brief Month number (1-12), for fixed_date, nth_weekday_of_month, and last_weekday_of_month
     * rules.
     *
     * Null for easter_offset rules, whose month varies by year.
     */
    std::optional<int> month;

    /**
     * @brief Day of month (1-31), for fixed_date rules only.
     *
     * Null for every other rule kind.
     */
    std::optional<int> day;

    /**
     * @brief Weekday (0=Sunday..6=Saturday, std::chrono::weekday's c_encoding), for
     * nth_weekday_of_month and last_weekday_of_month rules.
     *
     * Null for fixed_date and easter_offset rules.
     */
    std::optional<int> weekday;

    /**
     * @brief Which occurrence of the weekday in the month (1-4), for nth_weekday_of_month rules
     * only.
     *
     * Null for every other rule kind (last_weekday_of_month always means the last one, no count
     * needed).
     */
    std::optional<int> occurrence;

    /**
     * @brief Signed day offset from Easter Sunday, for easter_offset rules only (e.g. -2 = Good
     * Friday, +1 = Easter Monday).
     *
     * Null for every other rule kind.
     */
    std::optional<int> day_offset;

    /**
     * @brief How this holiday is re-observed when it falls on a weekend, if at all.
     *
     * Soft-enum: none | nearest_weekday | roll_forward_to_monday, matching
     * ores::analytics::quant::domain::observance_shift.
     */
    std::string shift;

    /**
     * @brief First year this rule is active (inclusive).
     *
     * Null means active for every year up to effective_to (or indefinitely, if that is also null).
     */
    std::optional<int> effective_from;

    /**
     * @brief Last year this rule is active (inclusive).
     *
     * Null means active for every year from effective_from (or indefinitely, if that is also null).
     */
    std::optional<int> effective_to;

    /**
     * @brief Username of the person who last modified this calendar rule.
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
 * @brief Dispatch-key identifier for calendar_rule, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const calendar_rule&) {
    return "ores.refdata.calendar_rule";
}

}

#endif
