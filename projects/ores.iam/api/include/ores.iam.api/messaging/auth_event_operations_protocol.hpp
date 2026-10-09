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
#ifndef ORES_IAM_API_MESSAGING_AUTH_EVENT_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_AUTH_EVENT_OPERATIONS_PROTOCOL_HPP

#include <cstdint>
#include <string>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief One authentication event, as the log records it.
 *
 * The timestamps are ISO 8601 strings: that is the storage form, and the
 * screen reads them as text rather than as an instant.
 */
struct auth_event {
    std::string id;
    std::string event_time;
    std::string account_id;
    std::string event_type;
    std::string username;
    std::string session_id;
    std::string party_id;
    std::string error_detail;
};

/**
 * @brief A window over the caller's tenant authentication events.
 *
 * An empty filter does not filter. The window is newest first, so the
 * screen reads the most recent events without asking for an order.
 */
struct list_auth_events_request {
    using response_type = struct list_auth_events_response;
    static constexpr std::string_view nats_subject = "iam.v1.auth_events.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string account_id;
    std::string event_type;
    std::string from_time;
    std::string to_time;
    std::uint32_t limit = 200;
    std::uint32_t offset = 0;
};

struct list_auth_events_response {
    std::vector<auth_event> events;
    bool success = false;
    std::string message;
};

}

#endif
