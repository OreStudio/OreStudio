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
#ifndef ORES_REFDATA_API_DOMAIN_CALENDAR_EVENT_HPP
#define ORES_REFDATA_API_DOMAIN_CALENDAR_EVENT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief A dated diary entry on a calendar.
 *
 * One row per dated diary entry on a
 * [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][calendar]]: a
 * central-bank meeting, a scheduled data release, or an open-ended
 * other event. One table for all event kinds -- never a table per
 * calendar or per type (settled 2026-08-09, story Decision D1; see
 * [[id:41E0E1FB-1D84-47E0-A417-2633F31F0A2A][Calendar Events]]). The
 * diary_entry_type column classifies the entry via the open-ended
 * [[id:AFAF296D-2962-48CE-A6E1-BFD5229E16C5][diary_entry_type]]
 * vocabulary (holiday, central_bank_meeting, data_release,
 * other). Holidays themselves keep their existing machinery
 * (calendar_rules, calendar_exceptions, calendar_date) -- the
 * holiday type stays in the vocabulary so the whole classification
 * lives in one place, even though its physical home is elsewhere.
 *
 * Template/Instance: an event row is an *instance*; its template is the
 * (calendar, diary_entry_type, name) triple. A worked case: the FOMC's
 * eight regularly scheduled meetings per year are entered as a short run
 * of central_bank_meeting instances on the US.FOMC calendar,
 * transcribed from the Fed's published calendar with
 * source'federalreserve.gov'. Formulaic recurrence generation is
 * deferred; calendar_rules='s grammar can later feed a template link
 * if a consumer needs it.
 */
struct calendar_event final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this event row.
     *
     * Surrogate key -- a calendar can have many events, so there is no single natural key column.
     */
    boost::uuids::uuid id;

    /**
     * @brief The calendar this event belongs to.
     *
     * References [[id:C09DF2B2-0E14-4742-8BAC-5D5842069580][ores_refdata_calendars_tbl.code]] (soft
     * FK).
     */
    std::string calendar_code;

    /**
     * @brief The specific date this event occurs on.
     *
     * One event of a given diary_entry_type per (calendar, date) pair.
     */
    std::chrono::year_month_day event_date;

    /**
     * @brief What kind of entry this is: holiday, central_bank_meeting, data_release, or other
     * (open-ended).
     *
     * References
     * [[id:AFAF296D-2962-48CE-A6E1-BFD5229E16C5][ores_refdata_diary_entry_types_tbl.code]] (soft
     * FK).
     */
    std::string diary_entry_type;

    /**
     * @brief Human-readable event name (e.g. "FOMC meeting").
     *
     * Part of the row's template triple, (calendar, diary_entry_type, name), not of its unique
     * identity: two instances of one template differ by event_date, not by name.
     */
    std::string name;

    /**
     * @brief Free-text note about the event (e.g. the decision time, or a link to the statement).
     *
     * Purely informational; never interpreted by the resolution engine.
     */
    std::optional<std::string> description;

    /**
     * @brief Provenance of this row's date, when transcribed from a published source (e.g.
     * federalreserve.gov).
     *
     * Null for locally-authored events with no external source.
     */
    std::optional<std::string> source;

    /**
     * @brief Username of the person who last modified this calendar event.
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
 * @brief Dispatch-key identifier for calendar_event, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const calendar_event&) {
    return "ores.refdata.calendar_event";
}

}

#endif
