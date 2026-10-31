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
#ifndef ORES_REPORTING_API_MESSAGING_RUN_RESULT_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_RUN_RESULT_PROTOCOL_HPP

#include "ores.reporting.api/domain/run_result.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct run_result_key {
    boost::uuids::uuid id;
};

struct run_result_lookup {
    run_result_key key;
    std::optional<ores::reporting::domain::run_result> run_result;
};

struct run_results_filter {
    std::optional<boost::uuids::uuid> party_id;
    std::optional<boost::uuids::uuid> report_instance_id;
    std::optional<boost::uuids::uuid> report_definition_id;
    std::optional<std::string> subject_type;
    std::optional<std::string> subject_id;
    std::optional<std::string> measure;
    std::optional<std::string> currency;
    std::optional<std::optional<boost::uuids::uuid>> counterparty_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> report_instance_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> report_definition_id_one_of;
    std::optional<std::vector<std::string>> subject_type_one_of;
    std::optional<std::vector<std::string>> subject_id_one_of;
    std::optional<std::vector<std::string>> measure_one_of;
    std::optional<std::vector<std::string>> currency_one_of;
    std::optional<std::vector<boost::uuids::uuid>> counterparty_id_one_of;
};

struct run_result_event {
    boost::uuids::uuid event_id;
    run_result_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_run_results_request {
    using response_type = struct list_run_results_response;
    static constexpr std::string_view nats_subject = "reporting.v1.run_results.list";
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
    std::optional<run_results_filter> filter;
};

struct list_run_results_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::run_result> results;
    std::uint64_t total;
};

struct get_run_result_request {
    using response_type = struct get_run_result_response;
    static constexpr std::string_view nats_subject = "reporting.v1.run_results.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    run_result_key key;
};

struct get_run_result_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::run_result> run_result;
};

struct get_many_run_results_request {
    using response_type = struct get_many_run_results_response;
    static constexpr std::string_view nats_subject = "reporting.v1.run_results.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<run_result_key> keys;
};

struct get_many_run_results_response {
    ores::utility::domain::result result;
    std::vector<run_result_lookup> entries;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace run_result_event_subjects {
inline constexpr std::string_view created = "reporting.v1.run_results_events.created";
inline constexpr std::string_view updated = "reporting.v1.run_results_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.run_results_events.deleted";
}

}

#endif
