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
#ifndef ORES_COMMS_MESSAGING_HANDSHAKE_PROTOCOL_HPP
#define ORES_COMMS_MESSAGING_HANDSHAKE_PROTOCOL_HPP

#include <string>
#include <vector>
#include <cstdint>
#include <expected>
#include "ores.comms/messaging/frame.hpp"

namespace ores::comms::messaging {

/**
 * @brief Handshake request message sent by client to initiate connection.
 */
struct handshake_request final {
    std::uint16_t client_version_major;
    std::uint16_t client_version_minor;
    std::string client_identifier;

    /**
     * @brief Serialize to frame payload.
     */
    static std::vector<std::byte> serialize(handshake_request v);

    /**
     * @brief Deserialize from frame payload.
     */
    static std::expected<handshake_request, error_code>
    deserialize(std::span<const std::byte> data);
};

/**
 * @brief Handshake response message sent by server to client.
 */
struct handshake_response final {
    std::uint16_t server_version_major;
    std::uint16_t server_version_minor;
    bool version_compatible;
    std::string server_identifier;
    error_code status;

    /**
     * @brief Serialize to frame payload.
     */
    static std::vector<std::byte> serialize(handshake_response v);

    /**
     * @brief Deserialize from frame payload.
     */
    static std::expected<handshake_response, error_code>
    deserialize(std::span<const std::byte> data);
};

/**
 * @brief Handshake acknowledgment message sent by client to complete handshake.
 */
struct handshake_ack final {
    error_code status;

    /**
     * @brief Serialize to frame payload.
     */
    static std::vector<std::byte> serialize(handshake_ack v);

    /**
     * @brief Deserialize from frame payload.
     */
    static std::expected<handshake_ack, error_code>
    deserialize(std::span<const std::byte> data);
};

/**
 * @brief Error response message sent when request processing fails.
 */
struct error_response final {
    error_code code;
    std::string message;

    /**
     * @brief Serialize to frame payload.
     */
    static std::vector<std::byte> serialize(error_response v);

    /**
     * @brief Deserialize from frame payload.
     */
    static std::expected<error_response, error_code> deserialize(std::span<const std::byte> data);
};

/**
 * @brief Create a handshake request frame.
 */
frame create_handshake_request_frame(
    std::uint32_t sequence,
    const std::string& client_identifier);

/**
 * @brief Create a handshake response frame.
 */
frame create_handshake_response_frame(
    std::uint32_t sequence,
    bool version_compatible,
    const std::string& server_identifier,
    error_code status = error_code::none);

/**
 * @brief Create a handshake acknowledgment frame.
 */
frame create_handshake_ack_frame(
    std::uint32_t sequence,
    error_code status = error_code::none);

/**
 * @brief Create an error response frame.
 */
frame create_error_response_frame(
    std::uint32_t sequence,
    error_code code,
    const std::string& message);

}

#endif
