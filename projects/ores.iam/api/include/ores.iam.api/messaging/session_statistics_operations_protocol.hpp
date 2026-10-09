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
#ifndef ORES_IAM_API_MESSAGING_SESSION_STATISTICS_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_SESSION_STATISTICS_OPERATIONS_PROTOCOL_HPP

#include <cstdint>
#include <string>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief One day's session statistics, for one account.
 *
 * The duration is in seconds and the byte counts are totals over the day's
 * ended sessions. A day with no ended sessions has no row.
 */
struct session_statistics_row {
    std::string day;
    std::string account_id;
    std::uint64_t session_count = 0;
    double avg_duration_seconds = 0.0;
    std::uint64_t total_bytes_sent = 0;
    std::uint64_t total_bytes_received = 0;
    double avg_bytes_sent = 0.0;
    double avg_bytes_received = 0.0;
    std::uint64_t unique_countries = 0;
};

/**
 * @brief A window over the caller's tenant session statistics.
 *
 * An empty filter does not filter. The window is newest first, so the screen
 * reads the most recent days without asking for an order.
 */
struct get_session_statistics_request {
    using response_type = struct get_session_statistics_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_session_statistics";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string account_id;
    std::string from_time;
    std::string to_time;
    std::uint32_t limit = 100;
    std::uint32_t offset = 0;
};

struct get_session_statistics_response {
    std::vector<session_statistics_row> rows;
    bool success = false;
    std::string message;
};

}

#endif
