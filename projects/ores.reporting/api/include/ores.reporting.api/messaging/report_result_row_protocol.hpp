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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_RESULT_ROW_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_RESULT_ROW_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_result_row.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct report_result_row_key {
    boost::uuids::uuid id;
};

struct report_result_row_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid report_instance_id;
    std::chrono::system_clock::time_point as_of;
    std::string subject_type;
    boost::uuids::uuid subject_id;
    std::string measure;
    std::string currency;
    boost::uuids::uuid report_definition_id;
    double figure;
    std::string source;
};

struct report_result_row_change {
    report_result_row_write write;
    ores::utility::domain::precondition precondition;
};

struct report_result_row_removal {
    report_result_row_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct report_result_row_lookup {
    report_result_row_key key;
    std::optional<ores::reporting::domain::report_result_row> report_result_row;
};

struct report_result_rows_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct report_result_row_event {
    boost::uuids::uuid event_id;
    report_result_row_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_report_result_rows_request {
    using response_type = struct list_report_result_rows_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_result_rows.list";
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
    std::optional<report_result_rows_filter> filter;
    std::optional<std::string> as_of;
};

struct list_report_result_rows_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_result_row> report_result_rows;
    std::uint64_t total;
};

struct get_report_result_row_request {
    using response_type = struct get_report_result_row_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_result_rows.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_result_row_key key;
};

struct get_report_result_row_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_result_row> report_result_row;
};

struct get_many_report_result_rows_request {
    using response_type = struct get_many_report_result_rows_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_result_rows.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_result_row_key> keys;
};

struct get_many_report_result_rows_response {
    ores::utility::domain::result result;
    std::vector<report_result_row_lookup> entries;
};

struct put_report_result_row_request {
    using response_type = struct put_report_result_row_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_result_rows.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_result_row_change change;
    ores::utility::domain::change_intent intent;
};

struct put_report_result_row_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_result_row> report_result_row;
};

struct put_many_report_result_rows_request {
    using response_type = struct put_many_report_result_rows_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_result_rows.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_result_row_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_report_result_rows_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_result_row> report_result_rows;
};

struct delete_report_result_row_request {
    using response_type = struct delete_report_result_row_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_result_rows.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_result_row_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_report_result_row_response {
    ores::utility::domain::result result;
};

struct delete_many_report_result_rows_request {
    using response_type = struct delete_many_report_result_rows_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_result_rows.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_result_row_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_report_result_rows_response {
    ores::utility::domain::result result;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace report_result_row_event_subjects {
inline constexpr std::string_view created = "reporting.v1.report_result_rows_events.created";
inline constexpr std::string_view updated = "reporting.v1.report_result_rows_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.report_result_rows_events.deleted";
}

}

#endif
