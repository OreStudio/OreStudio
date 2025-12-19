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
#include "ores.comms/messaging/error_protocol.hpp"
#include "ores.comms/messaging/message_traits.hpp"

namespace ores::comms::messaging {

/**
 * @brief Compression support bitmask values.
 *
 * Used in handshake_request::supported_compression to indicate which
 * compression algorithms the client supports.
 */
constexpr std::uint8_t COMPRESSION_SUPPORT_ZLIB = 0x01;
constexpr std::uint8_t COMPRESSION_SUPPORT_GZIP = 0x02;
constexpr std::uint8_t COMPRESSION_SUPPORT_BZIP2 = 0x04;
constexpr std::uint8_t COMPRESSION_SUPPORT_ALL = 0x07;

/**
 * @brief Handshake request message sent by client to initiate connection.
 */
struct handshake_request final {
    std::uint16_t client_version_major;
    std::uint16_t client_version_minor;
    std::string client_identifier;
    /**
     * @brief Bitmask of compression types supported by the client.
     *
     * Bit 0 (0x01): zlib
     * Bit 1 (0x02): gzip
     * Bit 2 (0x04): bzip2
     *
     * A value of 0x00 means no compression support (or old client).
     * A value of 0x07 means all compression types are supported.
     */
    std::uint8_t supported_compression = 0;

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
     * @brief Compression type selected for this session.
     *
     * The server selects one compression type from the client's supported
     * types, or none if compression is not desired or not supported.
     * All subsequent frames in this session should use this compression type.
     */
    compression_type selected_compression = compression_type::none;

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
 * @brief Create a handshake request frame.
 *
 * @param sequence Frame sequence number
 * @param client_identifier Client identifier string
 * @param supported_compression Bitmask of supported compression types
 *        (default: 0 = no compression support)
 */
frame create_handshake_request_frame(
    std::uint32_t sequence,
    const std::string& client_identifier,
    std::uint8_t supported_compression = 0);

/**
 * @brief Create a handshake response frame.
 *
 * @param sequence Frame sequence number
 * @param version_compatible Whether the client version is compatible
 * @param server_identifier Server identifier string
 * @param status Error status (default: none)
 * @param selected_compression Compression type selected for this session
 *        (default: none)
 */
frame create_handshake_response_frame(
    std::uint32_t sequence,
    bool version_compatible,
    const std::string& server_identifier,
    error_code status = error_code::none,
    compression_type selected_compression = compression_type::none);

/**
 * @brief Create a handshake acknowledgment frame.
 */
frame create_handshake_ack_frame(
    std::uint32_t sequence,
    error_code status = error_code::none);

/**
 * @brief Create an error response frame.
 *
 * @param sequence The sequence number for the frame
 * @param correlation_id The correlation ID from the request (for matching)
 * @param code The error code
 * @param message Human-readable error message
 */
frame create_error_response_frame(
    std::uint32_t sequence,
    std::uint32_t correlation_id,
    error_code code,
    const std::string& message);

/**
 * @brief Select a compression type from the client's supported types.
 *
 * Returns the preferred compression type that is supported by the client,
 * or compression_type::none if no compression is supported.
 *
 * @param supported_compression Bitmask of supported compression types
 * @param preferred The server's preferred compression type (default: zlib)
 * @return The selected compression type
 */
compression_type select_compression(
    std::uint8_t supported_compression,
    compression_type preferred = compression_type::zlib);

/**
 * @brief Message traits specialization for handshake_request.
 */
template<>
struct message_traits<handshake_request> {
    using request_type = handshake_request;
    using response_type = handshake_response;
    static constexpr message_type request_message_type =
        message_type::handshake_request;
};

}

#endif
