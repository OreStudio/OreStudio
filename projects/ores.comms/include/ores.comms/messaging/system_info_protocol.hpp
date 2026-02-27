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
#ifndef ORES_COMMS_MESSAGING_SYSTEM_INFO_PROTOCOL_HPP
#define ORES_COMMS_MESSAGING_SYSTEM_INFO_PROTOCOL_HPP

#include <span>
#include <string>
#include <vector>
#include <expected>
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.utility/serialization/error_code.hpp"

namespace ores::comms::messaging {

/**
 * @brief A single key-value pair carrying system metadata.
 */
struct system_info_entry final {
    std::string key;
    std::string value;
};

/**
 * @brief Request to retrieve system information from the server.
 *
 * An empty request — the server responds with all available KVPs.
 * No authentication required; safe to call immediately after connection.
 */
struct system_info_request final {
    /**
     * @brief Serialize to frame payload.
     */
    static std::vector<std::byte> serialize(system_info_request v);

    /**
     * @brief Deserialize from frame payload.
     */
    static std::expected<system_info_request, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

/**
 * @brief Server response carrying system information as key-value pairs.
 *
 * Entries are grouped by prefix:
 * - "server.*"   — compile-time server version and protocol info
 * - "database.*" — schema version and build metadata from the database
 */
struct system_info_response final {
    std::vector<system_info_entry> entries;

    /**
     * @brief Serialize to frame payload.
     */
    static std::vector<std::byte> serialize(system_info_response v);

    /**
     * @brief Deserialize from frame payload.
     */
    static std::expected<system_info_response, ores::utility::serialization::error_code>
    deserialize(std::span<const std::byte> data);
};

template<>
struct message_traits<system_info_request> {
    using request_type = system_info_request;
    using response_type = system_info_response;
    static constexpr message_type request_message_type =
        message_type::get_system_info_request;
};

}

#endif
