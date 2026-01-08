/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_IAM_MESSAGING_SESSION_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_SESSION_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <expected>
#include <cstdint>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.iam/domain/session.hpp"

namespace ores::iam::messaging {

/**
 * @brief Request to list sessions for an account.
 *
 * If account_id is nil UUID, returns sessions for the requesting user.
 * If account_id is specified and requester is admin, returns sessions for that account.
 */
struct list_sessions_request final {
    boost::uuids::uuid account_id;  // nil = own sessions
    std::uint32_t limit = 100;      // Max sessions to return
    std::uint32_t offset = 0;       // Pagination offset

    std::vector<std::byte> serialize() const;
    static std::expected<list_sessions_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const list_sessions_request& v);

/**
 * @brief Response containing session history.
 */
struct list_sessions_response final {
    std::vector<domain::session> sessions;
    std::uint32_t total_count = 0;  // Total available (for pagination)

    std::vector<std::byte> serialize() const;
    static std::expected<list_sessions_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const list_sessions_response& v);

/**
 * @brief Request to get session statistics.
 *
 * If account_id is nil UUID, returns aggregate statistics across all accounts.
 * If account_id is specified, returns statistics for that account.
 */
struct get_session_statistics_request final {
    boost::uuids::uuid account_id;  // nil = aggregate
    std::chrono::system_clock::time_point start_time;
    std::chrono::system_clock::time_point end_time;

    std::vector<std::byte> serialize() const;
    static std::expected<get_session_statistics_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_session_statistics_request& v);

/**
 * @brief Response containing session statistics.
 */
struct get_session_statistics_response final {
    std::vector<domain::session_statistics> statistics;

    std::vector<std::byte> serialize() const;
    static std::expected<get_session_statistics_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_session_statistics_response& v);

/**
 * @brief Request to get all active sessions.
 *
 * Admin-only: Returns all currently active sessions across all accounts.
 * Non-admin: Returns only the requesting user's active sessions.
 */
struct get_active_sessions_request final {
    std::vector<std::byte> serialize() const;
    static std::expected<get_active_sessions_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_active_sessions_request& v);

/**
 * @brief Response containing active sessions.
 */
struct get_active_sessions_response final {
    std::vector<domain::session> sessions;

    std::vector<std::byte> serialize() const;
    static std::expected<get_active_sessions_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_active_sessions_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits specialization for list_sessions_request.
 */
template<>
struct message_traits<iam::messaging::list_sessions_request> {
    using request_type = iam::messaging::list_sessions_request;
    using response_type = iam::messaging::list_sessions_response;
    static constexpr message_type request_message_type =
        message_type::list_sessions_request;
};

/**
 * @brief Message traits specialization for get_session_statistics_request.
 */
template<>
struct message_traits<iam::messaging::get_session_statistics_request> {
    using request_type = iam::messaging::get_session_statistics_request;
    using response_type = iam::messaging::get_session_statistics_response;
    static constexpr message_type request_message_type =
        message_type::get_session_statistics_request;
};

/**
 * @brief Message traits specialization for get_active_sessions_request.
 */
template<>
struct message_traits<iam::messaging::get_active_sessions_request> {
    using request_type = iam::messaging::get_active_sessions_request;
    using response_type = iam::messaging::get_active_sessions_response;
    static constexpr message_type request_message_type =
        message_type::get_active_sessions_request;
};

}

#endif
