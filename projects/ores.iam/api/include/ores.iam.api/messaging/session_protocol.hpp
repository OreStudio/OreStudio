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
#ifndef ORES_IAM_MESSAGING_SESSION_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_SESSION_PROTOCOL_HPP

#include "ores.iam.api/domain/session.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <string>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief Aggregated session statistics for a time period, computed from
 * the sessions hypertable's continuous aggregates.
 */
struct session_statistics {
    std::chrono::system_clock::time_point period_start;
    std::chrono::system_clock::time_point period_end;
    boost::uuids::uuid account_id;
    std::uint64_t session_count = 0;
    double avg_duration_seconds = 0.0;
    std::uint64_t total_bytes_sent = 0;
    std::uint64_t total_bytes_received = 0;
    double avg_bytes_sent = 0.0;
    double avg_bytes_received = 0.0;
    std::uint32_t unique_countries = 0;
};

/**
 * @brief A session with its party-scoped context.
 *
 * The session is the entity; party_id, visible_party_ids and username are
 * the denormalised fields reached through the account-party association.
 * They are message fields because no column backs them.
 */
struct session_view {
    ores::iam::domain::session session;
    std::string party_id;
    std::vector<std::string> visible_party_ids;
    std::string username;
};

struct list_sessions_request {
    using response_type = struct list_sessions_response;
    static constexpr std::string_view nats_subject = "iam.v1.sessions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string account_id;
    int limit = 50;
    int offset = 0;
};

struct list_sessions_response {
    std::vector<ores::iam::domain::session> sessions;
    int total_count = 0;
    bool success = false;
    std::string message;
};

struct get_active_sessions_request {
    using response_type = struct get_active_sessions_response;
    static constexpr std::string_view nats_subject = "iam.v1.sessions.active";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct get_active_sessions_response {
    std::vector<ores::iam::domain::session> sessions;
    bool success = false;
    std::string message;
};

struct get_session_statistics_request {
    std::string account_id;
    std::chrono::system_clock::time_point start_time;
    std::chrono::system_clock::time_point end_time;
};

struct get_session_statistics_response {
    std::vector<session_statistics> statistics;
    bool success = false;
    std::string message;
};

}

#endif
