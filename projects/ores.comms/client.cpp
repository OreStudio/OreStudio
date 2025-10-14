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
#include <print>
#include <boost/asio/connect.hpp>
#include "ores.comms/client.hpp"
#include "ores.comms/protocol/handshake.hpp"

namespace ores::comms {

client::client(client_config config, boost::asio::any_io_executor executor)
    : config_(std::move(config)),
      executor_(executor),
      ssl_ctx_(ssl::context::tlsv13_client),
      sequence_number_(0),
      connected_(false) {
    setup_ssl_context();
}

void client::setup_ssl_context() {
    if (config_.verify_certificate) {
        ssl_ctx_.set_verify_mode(ssl::verify_peer);
        ssl_ctx_.set_default_verify_paths();
    } else {
        ssl_ctx_.set_verify_mode(ssl::verify_none);
    }

    std::println("SSL context configured for client\n");
}

cobalt::promise<bool> client::connect() {
    try {
        std::println("Connecting to %s:%d\n", config_.host.c_str(), config_.port);

        // Resolve server address
        tcp::resolver resolver(executor_);
        auto endpoints = co_await resolver.async_resolve(
            config_.host,
            std::to_string(config_.port),
            cobalt::use_op);

        // Create TCP socket
        tcp::socket socket(executor_);
        co_await boost::asio::async_connect(socket, endpoints, cobalt::use_op);

        std::println("TCP connection established\n");

        // Create SSL socket
        connection::ssl_socket ssl_sock(std::move(socket), ssl_ctx_);

        // Create connection wrapper
        conn_ = std::make_unique<connection>(std::move(ssl_sock));

        // Perform SSL handshake
        co_await conn_->ssl_handshake_client();
        std::println("SSL handshake complete\n");

        // Perform protocol handshake
        bool handshake_ok = co_await perform_handshake();
        if (!handshake_ok) {
            std::println("Protocol handshake failed\n");
            disconnect();
            co_return false;
        }

        connected_ = true;
        std::println("Successfully connected to server\n");
        co_return true;

    } catch (const std::exception& e) {
        std::println("Connection error: %s\n", e.what());
        disconnect();
        co_return false;
    }
}

cobalt::promise<bool> client::perform_handshake() {
    try {
        // Send handshake request
        auto request_frame = protocol::create_handshake_request_frame(
            ++sequence_number_,
            config_.client_identifier);

        co_await conn_->write_frame(request_frame);
        std::println("Sent handshake request (client: %s, version: %d.%d)\n",
                   config_.client_identifier.c_str(),
                   protocol::PROTOCOL_VERSION_MAJOR,
                   protocol::PROTOCOL_VERSION_MINOR);

        // Read handshake response
        auto response_frame_result = co_await conn_->read_frame();
        if (!response_frame_result) {
            std::println("Failed to read handshake response\n");
            co_return false;
        }

        const auto& response_frame = *response_frame_result;

        // Verify message type
        if (response_frame.header().type != protocol::message_type::handshake_response) {
            std::println("Expected handshake response, got message type %d\n",
                       static_cast<int>(response_frame.header().type));
            co_return false;
        }

        // Deserialize response
        auto response_result = protocol::handshake_response::deserialize(response_frame.payload());
        if (!response_result) {
            std::println("Failed to deserialize handshake response\n");
            co_return false;
        }

        const auto& response = *response_result;
        std::println("Received handshake response (server: %s, version: %d.%d, compatible: %s)\n",
                   response.server_identifier.c_str(),
                   response.server_version_major,
                   response.server_version_minor,
                   response.version_compatible ? "yes" : "no");

        // Check compatibility
        if (!response.version_compatible) {
            std::println("Version incompatible with server\n");
            co_return false;
        }

        if (response.status != protocol::error_code::none) {
            std::println("Server reported error: %d\n", static_cast<int>(response.status));
            co_return false;
        }

        // Send acknowledgment
        auto ack_frame = protocol::create_handshake_ack_frame(
            ++sequence_number_,
            protocol::error_code::none);

        co_await conn_->write_frame(ack_frame);
        std::println("Sent handshake acknowledgment\n");

        co_return true;

    } catch (const std::exception& e) {
        std::println("Handshake exception: %s\n", e.what());
        co_return false;
    }
}

void client::disconnect() {
    if (conn_) {
        conn_->close();
        conn_.reset();
    }
    connected_ = false;
    std::println("Disconnected from server\n");
}

bool client::is_connected() const {
    return connected_ && conn_ && conn_->is_open();
}

}
