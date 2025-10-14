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
#include <iostream>

namespace ores::comms {

session::session(std::unique_ptr<connection> conn, std::string server_id)
    : conn_(std::move(conn)),
      server_id_(std::move(server_id)),
      sequence_number_(0),
      handshake_complete_(false) {}

cobalt::promise<void> session::run() {
    try {
        std::printf("Session started for client: %s\n", conn_->remote_address().c_str());

        // Perform SSL handshake
        co_await conn_->ssl_handshake_server();

        // Perform protocol handshake
        bool handshake_ok = co_await perform_handshake();
        if (!handshake_ok) {
            std::printf("Handshake failed for client: %s\n", conn_->remote_address().c_str());
            co_return;
        }

        std::printf("Handshake complete for client: %s\n", conn_->remote_address().c_str());

        // Process messages
        co_await process_messages();

    } catch (const std::exception& e) {
        std::printf("Session error for %s: %s\n", conn_->remote_address().c_str(), e.what());
    }

    conn_->close();
    std::printf("Session ended for client: %s\n", conn_->remote_address().c_str());
}

cobalt::promise<bool> session::perform_handshake() {
    try {
        // Read handshake request from client
        auto frame_result = co_await conn_->read_frame();
        if (!frame_result) {
            std::printf("Failed to read handshake request: error code %d\n",
                       static_cast<int>(frame_result.error()));
            co_return false;
        }

        const auto& request_frame = *frame_result;

        // Verify it's a handshake request
        if (request_frame.header().type != protocol::message_type::handshake_request) {
            std::printf("Expected handshake request, got message type %d\n",
                       static_cast<int>(request_frame.header().type));
            co_return false;
        }

        // Deserialize handshake request
        auto request_result = protocol::handshake_request::deserialize(request_frame.payload());
        if (!request_result) {
            std::printf("Failed to deserialize handshake request\n");
            co_return false;
        }

        const auto& request = *request_result;
        std::printf("Handshake request from client '%s' (version %d.%d)\n",
                   request.client_identifier.c_str(),
                   request.client_version_major,
                   request.client_version_minor);

        // Check version compatibility
        bool version_compatible = (request.client_version_major == protocol::PROTOCOL_VERSION_MAJOR);

        // Send handshake response
        auto response_frame = protocol::create_handshake_response_frame(
            ++sequence_number_,
            version_compatible,
            server_id_,
            version_compatible ? protocol::error_code::none : protocol::error_code::version_mismatch);

        co_await conn_->write_frame(response_frame);

        if (!version_compatible) {
            std::printf("Version mismatch: client=%d.%d, server=%d.%d\n",
                       request.client_version_major, request.client_version_minor,
                       protocol::PROTOCOL_VERSION_MAJOR, protocol::PROTOCOL_VERSION_MINOR);
            co_return false;
        }

        // Read handshake acknowledgment
        auto ack_frame_result = co_await conn_->read_frame();
        if (!ack_frame_result) {
            std::printf("Failed to read handshake ack\n");
            co_return false;
        }

        const auto& ack_frame = *ack_frame_result;
        if (ack_frame.header().type != protocol::message_type::handshake_ack) {
            std::printf("Expected handshake ack, got message type %d\n",
                       static_cast<int>(ack_frame.header().type));
            co_return false;
        }

        auto ack_result = protocol::handshake_ack::deserialize(ack_frame.payload());
        if (!ack_result) {
            std::printf("Failed to deserialize handshake ack\n");
            co_return false;
        }

        if (ack_result->status != protocol::error_code::none) {
            std::printf("Client reported handshake error: %d\n",
                       static_cast<int>(ack_result->status));
            co_return false;
        }

        handshake_complete_ = true;
        co_return true;

    } catch (const std::exception& e) {
        std::printf("Handshake exception: %s\n", e.what());
        co_return false;
    }
}

cobalt::promise<void> session::process_messages() {
    // Placeholder for future message processing
    // For now, just keep the connection alive
    std::printf("Session ready to process messages (not yet implemented)\n");
    co_return;
}

}
