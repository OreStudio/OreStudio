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
#ifndef ORES_IAM_MESSAGING_SESSION_SAMPLES_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_SESSION_SAMPLES_PROTOCOL_HPP

#include <span>
#include <string>
#include <vector>
#include <cstdint>
#include <expected>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"

namespace ores::iam::messaging {

/**
 * @brief Request to retrieve time-series samples for a session.
 *
 * The requesting user must own the session or have admin privileges.
 */
struct get_session_samples_request final {
    boost::uuids::uuid session_id;

    std::vector<std::byte> serialize() const;
    static std::expected<get_session_samples_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

/**
 * @brief A single time-series sample transferred over the wire.
 */
struct session_sample_dto final {
    /**
     * @brief Sample time as milliseconds since UNIX epoch.
     */
    std::uint64_t sample_time_ms = 0;

    /**
     * @brief Cumulative bytes sent at this sample.
     */
    std::uint64_t bytes_sent = 0;

    /**
     * @brief Cumulative bytes received at this sample.
     */
    std::uint64_t bytes_received = 0;

    /**
     * @brief RTT reported by the client in this ping, in milliseconds.
     */
    std::uint64_t latency_ms = 0;
};

/**
 * @brief Response containing time-series samples for a session.
 */
struct get_session_samples_response final {
    bool success = false;
    std::string message;
    std::vector<session_sample_dto> samples;

    std::vector<std::byte> serialize() const;
    static std::expected<get_session_samples_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

}

namespace ores::comms::messaging {

template<>
struct message_traits<ores::iam::messaging::get_session_samples_request> {
    using response_type = ores::iam::messaging::get_session_samples_response;
    static constexpr message_type request_message_type =
        message_type::get_session_samples_request;
};

}

#endif
