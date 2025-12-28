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
#include "ores.comms/service/handshake_service.hpp"

#include <format>
#include "ores.comms/net/connection.hpp"
#include "ores.comms/net/connection_error.hpp"
#include "ores.comms/messaging/handshake_protocol.hpp"
#include "ores.comms/messaging/message_types.hpp"

namespace ores::comms::service {

using messaging::message_type;
using messaging::error_code;
using messaging::handshake_request;
using messaging::handshake_response;
using messaging::handshake_ack;
using messaging::create_handshake_request_frame;
using messaging::create_handshake_response_frame;
using messaging::create_handshake_ack_frame;
using messaging::select_compression;
using messaging::PROTOCOL_VERSION_MAJOR;
using messaging::PROTOCOL_VERSION_MINOR;

using namespace ores::telemetry::log;

boost::asio::awaitable<messaging::compression_type> handshake_service::perform_client_handshake(
    net::connection& conn,
    const std::function<std::uint32_t()>& sequence_generator,
    const std::string& client_identifier,
    std::uint8_t supported_compression) {

    // Send handshake request
    auto request_frame = create_handshake_request_frame(
        sequence_generator(), client_identifier, supported_compression);

    BOOST_LOG_SEV(lg(), debug) << "About to send handshake request frame.";
    co_await conn.write_frame(request_frame);
    BOOST_LOG_SEV(lg(), info) << "Sent handshake request. Client: "
                              << client_identifier
                              << " Version: "
                              << PROTOCOL_VERSION_MAJOR
                              << "."
                              << PROTOCOL_VERSION_MINOR
                              << " Compression support: 0x"
                              << std::hex << static_cast<int>(supported_compression)
                              << std::dec;

    // Read handshake response
    BOOST_LOG_SEV(lg(), debug) << "About to read handshake response frame.";
    auto response_frame_result = co_await conn.read_frame();
    if (!response_frame_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to read handshake response. "
                                   << " Error code: "
                                   << static_cast<int>(response_frame_result.error());
        throw net::connection_error("Failed to read handshake response from server");
    }

    const auto& response_frame = *response_frame_result;

    // Verify message type
    if (response_frame.header().type != message_type::handshake_response) {
        BOOST_LOG_SEV(lg(), error) << "Expected handshake response, got message type "
                                   << response_frame.header().type;
        throw net::connection_error(std::format(
            "Unexpected message type during handshake: {}",
            static_cast<int>(response_frame.header().type)));
    }

    // Deserialize response
    auto response_result = handshake_response::deserialize(response_frame.payload());
    if (!response_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize handshake response";
        throw net::connection_error("Failed to deserialize handshake response");
    }

    const auto& response = *response_result;
    BOOST_LOG_SEV(lg(), info) << "Received handshake response. Server: "
                              << response.server_identifier << " version: "
                              << response.server_version_major
                              << "." << response.server_version_minor
                              << ". Compatible: "
                              << response.version_compatible
                              << ". Compression: "
                              << response.selected_compression;

    // Check version compatibility
    if (!response.version_compatible) {
        std::string error_msg = std::format(
            "Incompatible protocol version. Server: {}.{}, Client: {}.{}",
            response.server_version_major, response.server_version_minor,
            PROTOCOL_VERSION_MAJOR, PROTOCOL_VERSION_MINOR);
        BOOST_LOG_SEV(lg(), error) << error_msg;
        throw net::connection_error(error_msg);
    }

    // Check error status
    if (response.status != error_code::none) {
        BOOST_LOG_SEV(lg(), error) << "Server reported error " << response.status;
        throw net::connection_error(std::format(
            "Server rejected handshake with error code: {}",
            static_cast<int>(response.status)));
    }

    // Send handshake acknowledgment
    auto ack_frame = create_handshake_ack_frame(sequence_generator(), error_code::none);

    BOOST_LOG_SEV(lg(), debug) << "About to send handshake acknowledgement frame.";
    co_await conn.write_frame(ack_frame);
    BOOST_LOG_SEV(lg(), debug) << "Sent handshake acknowledgement.";

    BOOST_LOG_SEV(lg(), info) << "Client handshake completed successfully. "
                              << "Session compression: " << response.selected_compression;

    co_return response.selected_compression;
}

boost::asio::awaitable<std::optional<handshake_result>>
handshake_service::perform_server_handshake(
    net::connection& conn,
    std::uint32_t sequence,
    const std::string& server_identifier) {

    try {
        BOOST_LOG_SEV(lg(), debug) << "Starting server handshake process...";

        // Read handshake request from client
        // Use lenient reading (skip_version_check=true) to allow reading frames
        // with mismatched protocol versions. This enables us to send a proper
        // handshake_response with version details instead of rejecting the frame.
        BOOST_LOG_SEV(lg(), debug) << "About to read handshake request frame from client";
        auto frame_result = co_await conn.read_frame(true);  // skip_version_check=true
        if (!frame_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to read handshake request: error code "
                                      << static_cast<int>(frame_result.error());
            co_return std::nullopt;
        }

        const auto& request_frame = *frame_result;

        // Verify it's a handshake request
        if (request_frame.header().type != message_type::handshake_request) {
            BOOST_LOG_SEV(lg(), error) << "Expected handshake request, got message type "
                                      << static_cast<int>(request_frame.header().type);
            co_return std::nullopt;
        }

        BOOST_LOG_SEV(lg(), debug) << "Received valid handshake request frame";

        // Deserialize handshake request
        auto request_result = handshake_request::deserialize(request_frame.payload());
        if (!request_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize handshake request";
            co_return std::nullopt;
        }

        const auto& request = *request_result;
        BOOST_LOG_SEV(lg(), info) << "Handshake request from client '" << request.client_identifier
                                 << "' (version " << request.client_version_major << "."
                                 << request.client_version_minor << ")"
                                 << " compression support: 0x"
                                 << std::hex << static_cast<int>(request.supported_compression)
                                 << std::dec;

        // Check version compatibility
        bool version_compatible = (request.client_version_major == PROTOCOL_VERSION_MAJOR);

        // Select compression algorithm based on client support
        auto selected_compression = messaging::select_compression(
            request.supported_compression);

        BOOST_LOG_SEV(lg(), info) << "Compression negotiation: selected "
                                 << selected_compression;

        // Send handshake response
        auto response_frame = create_handshake_response_frame(
            sequence,
            version_compatible,
            server_identifier,
            version_compatible ? error_code::none : error_code::version_mismatch,
            selected_compression);

        BOOST_LOG_SEV(lg(), debug) << "About to send handshake response frame";
        co_await conn.write_frame(response_frame);
        BOOST_LOG_SEV(lg(), debug) << "Sent handshake response frame";

        if (!version_compatible) {
            BOOST_LOG_SEV(lg(), error) << "Version mismatch: client=" << request.client_version_major
                                      << "." << request.client_version_minor << ", server="
                                      << PROTOCOL_VERSION_MAJOR << "."
                                      << PROTOCOL_VERSION_MINOR;
            co_return std::nullopt;
        }

        // Read handshake acknowledgment
        BOOST_LOG_SEV(lg(), debug) << "About to read handshake acknowledgment frame from client";
        auto ack_frame_result = co_await conn.read_frame(false);
        if (!ack_frame_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to read handshake ack, error code: "
                                      << static_cast<int>(ack_frame_result.error());
            co_return std::nullopt;
        }

        const auto& ack_frame = *ack_frame_result;
        if (ack_frame.header().type != message_type::handshake_ack) {
            BOOST_LOG_SEV(lg(), error) << "Expected handshake ack, got message type "
                                      << static_cast<int>(ack_frame.header().type);
            co_return std::nullopt;
        }

        BOOST_LOG_SEV(lg(), debug) << "Received valid handshake acknowledgment frame";

        auto ack_result = handshake_ack::deserialize(ack_frame.payload());
        if (!ack_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to deserialize handshake ack";
            co_return std::nullopt;
        }

        const auto& ack = *ack_result;
        if (ack.status != error_code::none) {
            BOOST_LOG_SEV(lg(), error) << "Client reported handshake error: "
                                      << static_cast<int>(ack.status);
            co_return std::nullopt;
        }

        BOOST_LOG_SEV(lg(), info) << "Server handshake completed successfully. "
                                 << "Session compression: " << selected_compression;

        // Return full handshake result including client info
        co_return handshake_result{
            .compression = selected_compression,
            .client_identifier = request.client_identifier,
            .client_version_major = request.client_version_major,
            .client_version_minor = request.client_version_minor
        };

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Exception during server handshake: " << e.what();
        co_return std::nullopt;
    }
}

}
