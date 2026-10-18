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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_ERROR_EVENT_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_ERROR_EVENT_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_error_event.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct report_error_event_key {
    boost::uuids::uuid id;
};

struct report_error_event_lookup {
    report_error_event_key key;
    std::optional<ores::reporting::domain::report_error_event> report_error_event;
};

struct report_error_events_filter {
    std::optional<boost::uuids::uuid> report_instance_id;
    std::optional<std::string> severity;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> report_instance_id_one_of;
    std::optional<std::vector<std::string>> severity_one_of;
};

struct report_error_event_event {
    boost::uuids::uuid event_id;
    report_error_event_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_report_error_events_request {
    using response_type = struct list_report_error_events_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_events.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_error_events_filter> filter;
};

struct list_report_error_events_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_error_event> errors;
    std::uint64_t total;
};

struct get_report_error_event_request {
    using response_type = struct get_report_error_event_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_events.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_error_event_key key;
};

struct get_report_error_event_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_error_event> report_error_event;
};

struct get_many_report_error_events_request {
    using response_type = struct get_many_report_error_events_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_error_events.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_error_event_key> keys;
};

struct get_many_report_error_events_response {
    ores::utility::domain::result result;
    std::vector<report_error_event_lookup> entries;
};

struct list_by_report_instance_id_report_error_events_request {
    using response_type = struct list_by_report_instance_id_report_error_events_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.report_error_events.list_by_report_instance_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid report_instance_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_error_events_filter> filter;
};

struct list_by_report_instance_id_report_error_events_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_error_event> errors;
    std::uint64_t total;
};

struct report_error_count {
    /** @brief The run the counts are of. */
    boost::uuids::uuid report_instance_id;
    /** @brief How many of the run's problems are errors. */
    std::int64_t errors = 0;
    /** @brief How many are warnings. */
    std::int64_t warnings = 0;
};

/**
 * @brief Counts the problems of each named run in one call.
 *
 * The caller asks once with the runs a read of the latest runs returned, so a
 * list of forty definitions pays one row per run rather than every error of
 * every run. The request names no party: the session scopes the answer through
 * the row-level security on the table.
 */
struct get_report_error_counts_request {
    using response_type = struct get_report_error_counts_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.get_report_error_counts";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The runs to count the problems of, in any order. */
    std::vector<boost::uuids::uuid> report_instance_ids;
};

struct get_report_error_counts_response {
    ores::utility::domain::result result;
    /**
     * @brief One entry per run that recorded a problem, and no entry for a run
     * that recorded none.
     *
     * The absence is the screen's zero: nothing was found against that run.
     */
    std::vector<report_error_count> counts;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace report_error_event_event_subjects {
inline constexpr std::string_view created = "reporting.v1.report_error_events_events.created";
inline constexpr std::string_view updated = "reporting.v1.report_error_events_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.report_error_events_events.deleted";
}

}

#endif
