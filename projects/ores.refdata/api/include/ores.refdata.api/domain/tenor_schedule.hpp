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
#ifndef ORES_REFDATA_API_DOMAIN_TENOR_SCHEDULE_HPP
#define ORES_REFDATA_API_DOMAIN_TENOR_SCHEDULE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief Named schedule axis for tenor resolution.
 *
 * A named schedule axis a [[id:9A2E4D6B-7C1F-4B8A-A5D3-2F6E9B1C4A87][tenor]]
 * resolves along (story Decision D2: anchor + calendar offset + n
 * steps). Two kinds today, distinguished by schedule_source:
 *
 * - CLOSED_FORM: the dates come from a closed-form rule evaluated
 *   code-side. ROLL_QUARTER is the only instance: the first business
 *   day after the 20th of March/June/September/December (the IMM
 *   quarterly rule).
 * - EVENT_LOOKUP: the dates come from
 *   [[id:B20050A5-1245-4944-A328-2A0893C92AEC][calendar_event]] rows on a
 *   named calendar, filtered by diary entry type. FOMC_MEETING is the
 *   only instance: central_bank_meeting events on US.FOMC.
 *
 * calendar_code and diary_entry_type are null for closed-form
 * schedules (no event store involved) and required for event-lookup
 * ones -- but the binding is documented, not enforced in the schema.
 */
struct tenor_schedule final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique schedule code.
     *
     * Examples: 'ROLL_QUARTER', 'FOMC_MEETING'.
     */
    std::string code;

    /**
     * @brief Human-readable name for the schedule.
     */
    std::string name;

    /**
     * @brief Detailed description of the schedule.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief How the schedule's dates are produced: CLOSED_FORM (a closed-form rule evaluated
     * code-side) or EVENT_LOOKUP (calendar_event rows on a named calendar).
     */
    std::string schedule_source;

    /**
     * @brief The calendar whose diary events drive this schedule (event-lookup schedules only).
     *
     * References [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][ores_refdata_calendars_tbl.code]] (soft
     * FK).
     */
    std::optional<std::string> calendar_code;

    /**
     * @brief The diary entry type whose events drive this schedule (event-lookup schedules only).
     *
     * References
     * [[id:AFAF296D-2962-48CE-A6E1-BFD5229E16C5][ores_refdata_diary_entry_types_tbl.code]] (soft
     * FK).
     */
    std::optional<std::string> diary_entry_type;

    /**
     * @brief Username of the person who last modified this tenor schedule.
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
 * @brief Dispatch-key identifier for tenor_schedule, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const tenor_schedule&) {
    return "ores.refdata.tenor_schedule";
}

}

#endif
