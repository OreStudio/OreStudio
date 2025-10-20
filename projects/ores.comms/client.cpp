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
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <boost/asio/connect.hpp>
#include <boost/asio/detached.hpp>
#include <mutex>
#include "ores.comms/client.hpp"
#include "ores.comms/protocol/handshake.hpp"
#include "ores.utility/log/logger.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.comms.client"));

}

namespace ores::comms {

std::ostream& operator<<(std::ostream& s, const client_options& v) {
    rfl::json::write(v, s);
    return(s);
}

client::client(client_options config)
    : config_(std::move(config)),
      io_ctx_(std::make_unique<boost::asio::io_context>()),
      executor_(io_ctx_->get_executor()),
      ssl_ctx_(boost::asio::ssl::context::tlsv13_client),
      sequence_number_(0), connected_(false) {
    BOOST_LOG_SEV(lg, info) << "Client options: " << config_;
    setup_ssl_context();
}

client::client(client_options config, boost::asio::any_io_executor executor)
    : config_(std::move(config)), executor_(std::move(executor)),
      ssl_ctx_(boost::asio::ssl::context::tlsv13_client),
      sequence_number_(0), connected_(false) {
    BOOST_LOG_SEV(lg, info) << "Client options: " << config_;
    setup_ssl_context();
}

void client::setup_ssl_context() {
    if (config_.verify_certificate) {
        ssl_ctx_.set_verify_mode(boost::asio::ssl::verify_peer);
        ssl_ctx_.set_default_verify_paths();
    } else {
        ssl_ctx_.set_verify_mode(boost::asio::ssl::verify_none);
    }

    BOOST_LOG_SEV(lg, info) << "SSL context configured for client";
}

boost::cobalt::task<bool> client::connect() {
    try {
        BOOST_LOG_SEV(lg, info) << "Connecting to " << config_.host << ":" << config_.port;

        // Get the cobalt executor from the current coroutine context
        auto exec = co_await boost::cobalt::this_coro::executor;

        // Resolve server address
        boost::asio::ip::tcp::resolver resolver(exec);
        auto endpoints = co_await resolver.async_resolve(
            config_.host,
            std::to_string(config_.port),
            boost::cobalt::use_op);

        // Create TCP socket and connect
        boost::asio::ip::tcp::socket socket(exec);
        co_await boost::asio::async_connect(socket, endpoints, boost::cobalt::use_op);

        BOOST_LOG_SEV(lg, info) << "TCP connection established.";

        // Create SSL socket
        conn_ = std::make_unique<connection>(
            connection::ssl_socket(std::move(socket), ssl_ctx_));

        // Perform SSL handshake
        co_await conn_->ssl_handshake_client();
        BOOST_LOG_SEV(lg, info) << "SSL handshake complete.";

        // Perform protocol handshake
        bool handshake_ok = co_await perform_handshake();
        if (!handshake_ok) {
            BOOST_LOG_SEV(lg, error) << "Protocol handshake failed.";
            disconnect();
            co_return false;
        }

        {
            std::lock_guard<std::mutex> guard{state_mutex_};
            connected_ = true;
        }
        BOOST_LOG_SEV(lg, info) << "Successfully connected to server.";
        co_return true;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Connection error: " << e.what();
        disconnect();
        co_return false;
    }
}

boost::cobalt::task<bool> client::perform_handshake() {
    try {
        // Send handshake request
        auto request_frame = protocol::create_handshake_request_frame(
            [this]() {
                std::lock_guard<std::mutex> guard{state_mutex_};
                return ++sequence_number_;
            }(),
            config_.client_identifier);

        BOOST_LOG_SEV(lg, debug) << "About to send handshake request frame.";
        co_await conn_->write_frame(request_frame);
        BOOST_LOG_SEV(lg, info) << "Sent handshake request. Client: "
                                << config_.client_identifier
                                << " Version: "
                                << protocol::PROTOCOL_VERSION_MAJOR
                                << "."
                                << protocol::PROTOCOL_VERSION_MINOR;

        // Read handshake response
        BOOST_LOG_SEV(lg, debug) << "About to read handshake response frame";
        auto response_frame_result = co_await conn_->read_frame();
        if (!response_frame_result) {
            BOOST_LOG_SEV(lg, error) << "Failed to read handshake response. "
                                     << " Error code: "
                                     << static_cast<int>(response_frame_result.error());
            co_return false;
        }

        const auto& response_frame = *response_frame_result;

        // Verify message type
        if (response_frame.header().type != protocol::message_type::handshake_response) {
            BOOST_LOG_SEV(lg, error) << "Expected handshake response, got message type: "
                                     << static_cast<int>(response_frame.header().type);
            co_return false;
        }

        // Deserialize response
        auto response_result = protocol::handshake_response::deserialize(response_frame.payload());
        if (!response_result) {
            BOOST_LOG_SEV(lg, error) << "Failed to deserialize handshake response";
            co_return false;
        }

        const auto& response = *response_result;
        BOOST_LOG_SEV(lg, info) << "Received handshake response (server: "
                                << response.server_identifier << " version: "
                                << response.server_version_major
                                << "." << response.server_version_minor
                                << ". Compatible: "
                                << response.version_compatible;

        // Check compatibility
        if (!response.version_compatible) {
            BOOST_LOG_SEV(lg, error) << "Version incompatible with server";
            co_return false;
        }

        if (response.status != protocol::error_code::none) {
            BOOST_LOG_SEV(lg, error) << "Server reported error: " << static_cast<int>(response.status);
            co_return false;
        }

        // Send acknowledgment
        auto ack_frame = protocol::create_handshake_ack_frame(
            [this]() {
                std::lock_guard<std::mutex> guard{state_mutex_};
                return ++sequence_number_;
            }(),
            protocol::error_code::none);

        BOOST_LOG_SEV(lg, debug) << "About to send handshake acknowledgment frame";
        co_await conn_->write_frame(ack_frame);
        BOOST_LOG_SEV(lg, info) << "Sent handshake acknowledgment";

        BOOST_LOG_SEV(lg, debug) << "Client handshake completed successfully";
        co_return true;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Handshake exception: " << e.what();
        co_return false;
    }
}

void client::disconnect() {
    {
        std::lock_guard<std::mutex> guard{state_mutex_};
        if (conn_) {
            conn_->close();
            conn_.reset();
        }
        connected_ = false;
    }
    BOOST_LOG_SEV(lg, info) << "Disconnected from server";
}

bool client::is_connected() const {
    std::lock_guard<std::mutex> guard{state_mutex_};
    return connected_ && conn_ && conn_->is_open();
}

boost::cobalt::task<std::expected<protocol::frame, protocol::error_code>>
client::send_request(protocol::frame request_frame) {
    {
        std::lock_guard<std::mutex> guard{state_mutex_};
        if (!is_connected()) {
            BOOST_LOG_SEV(lg, error) << "Cannot send request: not connected";
            co_return std::unexpected(protocol::error_code::network_error);
        }
    }

    try {
        // Update sequence number in the frame
        request_frame = protocol::frame(
            request_frame.header().type,
            [this]() {
                std::lock_guard<std::mutex> guard{state_mutex_};
                return ++sequence_number_;
            }(),
            std::vector<std::uint8_t>(request_frame.payload()));

        BOOST_LOG_SEV(lg, debug) << "Sending request frame, type: "
                                 << std::hex << static_cast<std::uint16_t>(request_frame.header().type);

        // Send request
        co_await conn_->write_frame(request_frame);

        // Read response
        auto response_result = co_await conn_->read_frame();
        if (!response_result) {
            BOOST_LOG_SEV(lg, error) << "Failed to read response frame, error: "
                                     << static_cast<int>(response_result.error());
            co_return std::unexpected(response_result.error());
        }

        BOOST_LOG_SEV(lg, debug) << "Received response frame, type: "
                                 << std::hex << static_cast<std::uint16_t>(response_result->header().type);

        co_return *response_result;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg, error) << "Request exception: " << e.what();
        co_return std::unexpected(protocol::error_code::network_error);
    }
}

bool client::connect_sync() {
    BOOST_LOG_SEV(lg, debug) << "connect_sync: starting";

    auto task = [this]() -> boost::cobalt::task<bool> {
        BOOST_LOG_SEV(lg, debug) << "connect_sync: task started";
        bool result = co_await connect();
        BOOST_LOG_SEV(lg, debug) << "connect_sync: task completed, result=" << result;
        co_return result;
    };

    BOOST_LOG_SEV(lg, debug) << "connect_sync: spawning task with use_future";
    auto future = boost::cobalt::spawn(executor_, task(), boost::asio::use_future);

    if (io_ctx_) {
        BOOST_LOG_SEV(lg, debug) << "connect_sync: running io_context";
        io_ctx_->run();
        io_ctx_->restart();
    }

    BOOST_LOG_SEV(lg, debug) << "connect_sync: getting result from future";
    bool result = future.get();
    BOOST_LOG_SEV(lg, debug) << "connect_sync: returning " << result;
    return result;
}

std::expected<protocol::frame, protocol::error_code>
client::send_request_sync(protocol::frame request_frame) {
    using result_t = std::expected<protocol::frame, protocol::error_code>;

    auto task = [ this, request_frame = std::move(request_frame) ]() mutable ->
        boost::cobalt::task<result_t> {
        result_t result = co_await send_request(std::move(request_frame));
        co_return result;
    };

    auto future = boost::cobalt::spawn(executor_, task(), boost::asio::use_future);

    if (io_ctx_) {
        io_ctx_->run();
        io_ctx_->restart();
    }

    return future.get();
}

}
