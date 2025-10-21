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
#include "ores.comms/session.hpp"
#include "ores.comms/protocol/handshake.hpp"
#include "ores.utility/log/logger.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.comms.session"));

}

namespace ores::comms {

session::session(std::unique_ptr<connection> conn, std::string server_id,
    std::shared_ptr<protocol::message_dispatcher> dispatcher)
    : conn_(std::move(conn)),
      server_id_(std::move(server_id)),
      dispatcher_(std::move(dispatcher)),
      sequence_number_(0),
      handshake_complete_(false) {}

boost::asio::awaitable<void> session::run() {
    std::string remote_addr = conn_->remote_address();
    try {
        BOOST_LOG_SEV(lg, info) << "Session started for client: " << remote_addr;

        // Perform SSL handshake
        co_await conn_->ssl_handshake_server();

        // Perform protocol handshake
        bool handshake_ok = co_await perform_handshake();
        if (!handshake_ok) {
            BOOST_LOG_SEV(lg, warn) << "Handshake failed for client: " << remote_addr;
            co_return;
        }

        BOOST_LOG_SEV(lg, info) << "Handshake complete for client: " << remote_addr;

        // Process messages
        co_await process_messages();

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Session error for " << remote_addr << ": " << e.what();
    }

    conn_->close();
    BOOST_LOG_SEV(lg, info) << "Session ended for client: " << remote_addr;
}

boost::asio::awaitable<bool> session::perform_handshake() {
    try {
        BOOST_LOG_SEV(lg, debug) << "Starting server handshake process...";

        // Read handshake request from client
        BOOST_LOG_SEV(lg, debug) << "About to read handshake request frame from client";
        auto frame_result = co_await conn_->read_frame();
        if (!frame_result) {
            BOOST_LOG_SEV(lg, error) << "Failed to read handshake request: error code "
                                      << static_cast<int>(frame_result.error());
            co_return false;
        }

        const auto& request_frame = *frame_result;

        // Verify it's a handshake request
        if (request_frame.header().type != protocol::message_type::handshake_request) {
            BOOST_LOG_SEV(lg, error) << "Expected handshake request, got message type "
                                      << static_cast<int>(request_frame.header().type);
            co_return false;
        }

        BOOST_LOG_SEV(lg, debug) << "Received valid handshake request frame";

        // Deserialize handshake request
        auto request_result = protocol::handshake_request::deserialize(request_frame.payload());
        if (!request_result) {
            BOOST_LOG_SEV(lg, error) << "Failed to deserialize handshake request";
            co_return false;
        }

        const auto& request = *request_result;
        BOOST_LOG_SEV(lg, info) << "Handshake request from client '" << request.client_identifier
                                 << "' (version " << request.client_version_major << "."
                                 << request.client_version_minor << ")";

        // Check version compatibility
        bool version_compatible = (request.client_version_major == protocol::PROTOCOL_VERSION_MAJOR);

        // Send handshake response
        auto response_frame = protocol::create_handshake_response_frame(
            ++sequence_number_,
            version_compatible,
            server_id_,
            version_compatible ? protocol::error_code::none : protocol::error_code::version_mismatch);

        BOOST_LOG_SEV(lg, debug) << "About to send handshake response frame";
        co_await conn_->write_frame(response_frame);
        BOOST_LOG_SEV(lg, debug) << "Sent handshake response frame";

        if (!version_compatible) {
            BOOST_LOG_SEV(lg, error) << "Version mismatch: client=" << request.client_version_major
                                      << "." << request.client_version_minor << ", server="
                                      << protocol::PROTOCOL_VERSION_MAJOR << "."
                                      << protocol::PROTOCOL_VERSION_MINOR;
            co_return false;
        }

        // Read handshake acknowledgment
        BOOST_LOG_SEV(lg, debug) << "About to read handshake acknowledgment frame from client";
        auto ack_frame_result = co_await conn_->read_frame();
        if (!ack_frame_result) {
            BOOST_LOG_SEV(lg, error) << "Failed to read handshake ack, error code: " << static_cast<int>(ack_frame_result.error());
            co_return false;
        }

        const auto& ack_frame = *ack_frame_result;
        if (ack_frame.header().type != protocol::message_type::handshake_ack) {
            BOOST_LOG_SEV(lg, error) << "Expected handshake ack, got message type "
                                      << static_cast<int>(ack_frame.header().type);
            co_return false;
        }

        BOOST_LOG_SEV(lg, debug) << "Received valid handshake acknowledgment frame";

        auto ack_result = protocol::handshake_ack::deserialize(ack_frame.payload());
        if (!ack_result) {
            BOOST_LOG_SEV(lg, error) << "Failed to deserialize handshake ack";
            co_return false;
        }

        if (ack_result->status != protocol::error_code::none) {
            BOOST_LOG_SEV(lg, error) << "Client reported handshake error: "
                                      << static_cast<int>(ack_result->status);
            co_return false;
        }

        handshake_complete_ = true;
        BOOST_LOG_SEV(lg, debug) << "Server handshake completed successfully";
        co_return true;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Handshake exception: " << e.what();
        co_return false;
    }
}

boost::asio::awaitable<void> session::process_messages() {
    BOOST_LOG_SEV(lg, debug) << "Starting message processing loop";

    try {
        while (true) {
            // Read next message frame
            auto frame_result = co_await conn_->read_frame();
            if (!frame_result) {
                auto err = frame_result.error();
                if (err == protocol::error_code::network_error) {
                    BOOST_LOG_SEV(lg, info) << "Client disconnected";
                } else {
                    BOOST_LOG_SEV(lg, error) << "Failed to read frame: "
                                              << static_cast<int>(err);
                }
                co_return;
            }

            const auto& request_frame = *frame_result;
            BOOST_LOG_SEV(lg, debug) << "Received message type "
                                      << std::hex << static_cast<std::uint16_t>(request_frame.header().type);

            // Dispatch to appropriate handler
            auto remote_addr = conn_->remote_address();
            auto response_result = co_await dispatcher_->dispatch(request_frame,
                ++sequence_number_, remote_addr);
            if (!response_result) {
                BOOST_LOG_SEV(lg, error) << "Message dispatch failed: "
                                          << static_cast<int>(response_result.error());
                // Optionally send error response frame here
                co_return;
            }

            // Send response back to client
            co_await conn_->write_frame(*response_result);
            BOOST_LOG_SEV(lg, debug) << "Sent response for message type "
                                      << std::hex << static_cast<std::uint16_t>(request_frame.header().type);
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Exception in message processing: " << e.what();
    }
}

}
