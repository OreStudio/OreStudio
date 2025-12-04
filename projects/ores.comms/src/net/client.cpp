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
#include "ores.comms/net/client.hpp"

#include <mutex>
#include <format>
#include <chrono>
#include <boost/asio/connect.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/dispatch.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/bind_executor.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/asio/use_future.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/log/sources/record_ostream.hpp>
#include "ores.comms/net/connection_error.hpp"
#include "ores.comms/messaging/handshake_service.hpp"
#include "ores.comms/messaging/heartbeat_protocol.hpp"

namespace ores::comms::net {

using namespace ores::utility::log;

void client::setup_ssl_context() {
    if (config_.verify_certificate) {
        ssl_ctx_.set_verify_mode(boost::asio::ssl::verify_peer);
        ssl_ctx_.set_default_verify_paths();
    } else {
        ssl_ctx_.set_verify_mode(boost::asio::ssl::verify_none);
    }

    BOOST_LOG_SEV(lg(), info) << "SSL context configured for client";
}

boost::asio::awaitable<void> client::perform_handshake() {
    auto sequence_generator = [this]() {
        std::lock_guard guard{state_mutex_};
        return ++sequence_number_;
    };

    co_await messaging::handshake_service::perform_client_handshake(
        *conn_, sequence_generator, config_.client_identifier);
}

client::client(client_options config)
    : config_(std::move(config)),
      io_ctx_(std::make_unique<boost::asio::io_context>()),
      executor_(io_ctx_->get_executor()),
      ssl_ctx_(boost::asio::ssl::context::tlsv13_client),
      sequence_number_(0), connected_(false) {
    BOOST_LOG_SEV(lg(), info) << "Client options: " << config_;
    setup_ssl_context();

    // Initialize write strand and pending request map
    write_strand_ = std::make_unique<boost::asio::strand<boost::asio::any_io_executor>>(
        boost::asio::make_strand(executor_));
    pending_requests_ = std::make_unique<pending_request_map>(executor_);
}

client::client(client_options config, boost::asio::any_io_executor executor)
    : config_(std::move(config)), executor_(std::move(executor)),
      ssl_ctx_(boost::asio::ssl::context::tlsv13_client),
      sequence_number_(0), connected_(false) {
    BOOST_LOG_SEV(lg(), info) << "Client options: " << config_;
    setup_ssl_context();

    // Initialize write strand and pending request map
    write_strand_ = std::make_unique<boost::asio::strand<boost::asio::any_io_executor>>(
        boost::asio::make_strand(executor_));
    pending_requests_ = std::make_unique<pending_request_map>(executor_);
}

boost::asio::awaitable<void> client::connect() {
    try {
        BOOST_LOG_SEV(lg(), info) << "Connecting to " << config_.host
                                  << ":" << config_.port;

        auto exec = co_await boost::asio::this_coro::executor;
        boost::asio::ip::tcp::resolver resolver(exec);
        auto endpoints = co_await resolver.async_resolve(
            config_.host,
            std::to_string(config_.port),
            boost::asio::use_awaitable);

        boost::asio::ip::tcp::socket socket(exec);
        co_await boost::asio::async_connect(socket, endpoints,
            boost::asio::use_awaitable);

        BOOST_LOG_SEV(lg(), debug) << "TCP connection established.";

        conn_ = std::make_unique<connection>(
            connection::ssl_socket(std::move(socket), ssl_ctx_));
        co_await conn_->ssl_handshake_client();
        BOOST_LOG_SEV(lg(), debug) << "SSL handshake complete.";

        BOOST_LOG_SEV(lg(), debug) << "Protocol version: "
                                   << messaging::PROTOCOL_VERSION_MAJOR << "."
                                   << messaging::PROTOCOL_VERSION_MINOR
                                   << " (client: " << config_.client_identifier << ")";

        co_await perform_handshake();
        {
            std::lock_guard guard{state_mutex_};
            connected_ = true;
        }
        BOOST_LOG_SEV(lg(), info) << "Successfully connected to server.";

        // Start message loop in background (single reader for all responses)
        // Only needed when using correlation-based request/response or heartbeat
        if (config_.heartbeat_enabled) {
            boost::asio::co_spawn(exec, run_message_loop(), boost::asio::detached);
            BOOST_LOG_SEV(lg(), debug) << "Message loop started";
        }

        // Start heartbeat in background
        if (config_.heartbeat_enabled) {
            boost::asio::co_spawn(exec, run_heartbeat(), boost::asio::detached);
            BOOST_LOG_SEV(lg(), info) << "Heartbeat enabled with interval: "
                                       << config_.heartbeat_interval_seconds << "s";
        } else {
            BOOST_LOG_SEV(lg(), info) << "Heartbeat disabled";
        }

    } catch (const connection_error&) {
        disconnect();
        throw;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Connection error: " << e.what();
        disconnect();
        throw connection_error(
            std::format("Failed to connect to server: {}", e.what()));
    }
}

void client::connect_sync() {
    BOOST_LOG_SEV(lg(), debug) << "Starting to connect synchronously.";

    auto task = [this]() -> boost::asio::awaitable<void> {
        BOOST_LOG_SEV(lg(), trace) << "connect_sync: task started.";
        co_await connect();
        BOOST_LOG_SEV(lg(), trace) << "connect_sync: task completed.";
    };

    BOOST_LOG_SEV(lg(), trace) << "connect_sync: spawning task with use_future.";
    auto future = boost::asio::co_spawn(executor_, task(), boost::asio::use_future);

    if (io_ctx_) {
        BOOST_LOG_SEV(lg(), trace) << "connect_sync: running io_context.";
        io_ctx_->run();
        io_ctx_->restart();
    }

    BOOST_LOG_SEV(lg(), debug) << "connect_sync: getting result from future.";
    future.get();
    BOOST_LOG_SEV(lg(), debug) << "Completed synchronous connect successfully.";
}

void client::disconnect() {
    BOOST_LOG_SEV(lg(), debug) << "Disconnecting from server";

    // First mark as disconnected to stop the message loop
    {
        std::lock_guard guard{state_mutex_};
        connected_ = false;
    }

    // Close the connection (this will cancel pending async operations)
    // Note: We don't reset the unique_ptr here because the message loop
    // may still be awaiting on the connection. The close will cause pending
    // reads to fail, and the message loop will exit gracefully.
    {
        std::lock_guard guard{state_mutex_};
        if (conn_) {
            conn_->close();
            // Don't reset conn_ here - let the message loop handle cleanup
        }
    }

    // Fail all pending requests so waiting coroutines can complete
    if (pending_requests_) {
        pending_requests_->fail_all(messaging::error_code::network_error);
    }

    BOOST_LOG_SEV(lg(), info) << "Disconnected from server";
}

bool client::is_connected() const {
    std::lock_guard guard{state_mutex_};
    return connected_ && conn_ && conn_->is_open();
}

void client::set_disconnect_callback(disconnect_callback_t callback) {
    std::lock_guard guard{state_mutex_};
    disconnect_callback_ = std::move(callback);
}

boost::asio::awaitable<void> client::run_heartbeat() {
    if (!config_.heartbeat_enabled) {
        BOOST_LOG_SEV(lg(), debug) << "Heartbeat disabled in configuration";
        co_return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Starting heartbeat loop with interval: "
                               << config_.heartbeat_interval_seconds << "s";

    try {
        auto exec = co_await boost::asio::this_coro::executor;
        boost::asio::steady_timer timer(exec);

        while (is_connected()) {
            // Wait for the configured interval
            timer.expires_after(std::chrono::seconds(config_.heartbeat_interval_seconds));
            co_await timer.async_wait(boost::asio::use_awaitable);

            if (!is_connected()) {
                break;
            }

            // Send ping and wait for pong using new infrastructure
            BOOST_LOG_SEV(lg(), trace) << "Sending heartbeat ping";

            // Get sequence and correlation ID
            std::uint32_t seq;
            {
                std::lock_guard guard{state_mutex_};
                if (!conn_ || !connected_) {
                    break;
                }
                seq = ++sequence_number_;
            }
            auto corr_id = next_correlation_id();

            // Register pending request before sending
            auto channel = pending_requests_->register_request(corr_id);

            // Create and send ping frame via write strand
            auto ping_frame = messaging::create_ping_frame(seq, corr_id);
            try {
                co_await write_frame(ping_frame);
            } catch (...) {
                pending_requests_->remove(corr_id);
                BOOST_LOG_SEV(lg(), warn) << "Failed to send ping";
                break;
            }

            BOOST_LOG_SEV(lg(), trace) << "Waiting for pong response (correlation_id="
                                       << corr_id << ")";

            // Wait for response from message loop
            auto response_result = co_await channel->get();

            if (!response_result) {
                BOOST_LOG_SEV(lg(), warn) << "Heartbeat failed - no pong received";
                // The message loop handles disconnect callback when it exits
                break;
            }

            const auto& response = *response_result;
            if (response.header().type != messaging::message_type::pong) {
                BOOST_LOG_SEV(lg(), warn) << "Heartbeat: expected pong, got "
                                          << response.header().type;
                break;
            }

            BOOST_LOG_SEV(lg(), trace) << "Heartbeat pong received";
        }
    } catch (const boost::system::system_error& e) {
        if (e.code() == boost::asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg(), debug) << "Heartbeat cancelled";
        } else {
            BOOST_LOG_SEV(lg(), error) << "Heartbeat error: " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Heartbeat loop ended";
}

boost::asio::awaitable<std::expected<messaging::frame, messaging::error_code>>
client::send_request(messaging::frame request_frame) {
    BOOST_LOG_SEV(lg(), debug) << "Sending request.";
    if (!is_connected()) {
        BOOST_LOG_SEV(lg(), error) << "Cannot send request: not connected";
        co_return std::unexpected(messaging::error_code::network_error);
    }
    BOOST_LOG_SEV(lg(), trace) << "Currently connected.";

    // Get sequence number
    std::uint32_t seq;
    {
        std::lock_guard guard{state_mutex_};
        seq = ++sequence_number_;
    }

    // If message loop is running, use correlation-based request/response
    if (message_loop_running_) {
        auto corr_id = next_correlation_id();
        auto channel = pending_requests_->register_request(corr_id);

        // Create frame with correlation ID
        auto frame_to_send = messaging::frame(
            request_frame.header().type, seq, corr_id,
            std::vector<std::byte>(request_frame.payload()));

        BOOST_LOG_SEV(lg(), debug) << "Sending request " << frame_to_send.header().type
                                   << " (correlation_id=" << corr_id << ")";

        try {
            co_await write_frame(frame_to_send);
        } catch (const std::exception& e) {
            pending_requests_->remove(corr_id);
            BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << e.what();
            co_return std::unexpected(messaging::error_code::network_error);
        }

        // Wait for response via pending_requests (message loop delivers it)
        auto response_result = co_await channel->get();

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Request failed: " << response_result.error();
            co_return std::unexpected(response_result.error());
        }

        BOOST_LOG_SEV(lg(), debug) << "Received response "
                                   << response_result->header().type;
        co_return *response_result;
    }

    // Message loop not running - use direct read/write
    try {
        auto frame_to_send = messaging::frame(
            request_frame.header().type, seq,
            std::vector<std::byte>(request_frame.payload()));

        BOOST_LOG_SEV(lg(), debug) << "Sending request (direct) "
                                   << frame_to_send.header().type;

        co_await conn_->write_frame(frame_to_send);

        auto response_result = co_await conn_->read_frame();
        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to read response frame, error "
                                       << response_result.error();
            {
                std::lock_guard guard{state_mutex_};
                connected_ = false;
            }
            if (conn_) {
                conn_->close();
            }
            BOOST_LOG_SEV(lg(), warn) << "Connection lost";
            co_return std::unexpected(response_result.error());
        }

        BOOST_LOG_SEV(lg(), debug) << "Received response "
                                   << response_result->header().type;
        co_return *response_result;

    } catch (const boost::system::system_error& e) {
        BOOST_LOG_SEV(lg(), error) << "Network error: " << e.what();
        {
            std::lock_guard guard{state_mutex_};
            connected_ = false;
        }
        if (conn_) {
            conn_->close();
        }
        co_return std::unexpected(messaging::error_code::network_error);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Request exception: " << e.what();
        co_return std::unexpected(messaging::error_code::network_error);
    }
}

std::expected<messaging::frame, messaging::error_code>
client::send_request_sync(messaging::frame request_frame) {
    BOOST_LOG_SEV(lg(), debug) << "Starting to send request synchronously.";

    using result_type = std::expected<messaging::frame, messaging::error_code>;

    auto task = [this, request_frame = std::move(
            request_frame)]() mutable -> boost::asio::awaitable<result_type> {
        BOOST_LOG_SEV(lg(), trace) << "send_request_sync: task started.";
        result_type result = co_await send_request(std::move(request_frame));
        BOOST_LOG_SEV(lg(), trace) << "send_request_sync: task completed.";
        co_return result;
    };

    BOOST_LOG_SEV(lg(), trace) << "send_request_sync: spawning task with use_future";
    auto future = boost::asio::co_spawn(executor_, task(), boost::asio::use_future);
    if (io_ctx_) {
        io_ctx_->run();
        io_ctx_->restart();
    }

    BOOST_LOG_SEV(lg(), debug) << "send_request_sync: getting result from future.";
    auto r = future.get();
    BOOST_LOG_SEV(lg(), debug) << "Completed synchronous connect successfully.";
    return r;
}

std::uint32_t client::next_correlation_id() {
    return correlation_id_counter_.fetch_add(1, std::memory_order_relaxed);
}

boost::asio::awaitable<void> client::write_frame(const messaging::frame& f) {
    // Execute write on the strand to serialize all writes
    co_await boost::asio::dispatch(
        boost::asio::bind_executor(*write_strand_, boost::asio::use_awaitable));

    // Now we're on the strand - safe to write
    co_await conn_->write_frame(f);
}

boost::asio::awaitable<void> client::run_message_loop() {
    BOOST_LOG_SEV(lg(), debug) << "Starting message loop";
    message_loop_running_ = true;

    try {
        while (is_connected()) {
            // Safely get connection pointer under lock
            connection* conn_ptr = nullptr;
            {
                std::lock_guard guard{state_mutex_};
                if (conn_) {
                    conn_ptr = conn_.get();
                }
            }

            if (!conn_ptr) {
                BOOST_LOG_SEV(lg(), debug) << "Connection gone, exiting message loop";
                break;
            }

            // Read the next frame
            auto frame_result = co_await conn_ptr->read_frame();

            if (!frame_result) {
                BOOST_LOG_SEV(lg(), warn) << "Message loop read error: "
                                          << frame_result.error();
                break;
            }

            const auto& frame = *frame_result;
            const auto msg_type = frame.header().type;
            const auto corr_id = frame.correlation_id();

            BOOST_LOG_SEV(lg(), trace) << "Message loop received " << msg_type
                                       << " correlation_id=" << corr_id;

            // Dispatch by message type
            switch (msg_type) {
            case messaging::message_type::pong:
            case messaging::message_type::error_response:
                // Response types - complete pending request
                if (!pending_requests_->complete(corr_id, frame)) {
                    BOOST_LOG_SEV(lg(), warn) << "No pending request for " << msg_type
                                              << " correlation_id=" << corr_id;
                }
                break;

            default:
                // All other responses (including domain-specific responses)
                if (!pending_requests_->complete(corr_id, frame)) {
                    BOOST_LOG_SEV(lg(), warn) << "Unexpected message type " << msg_type
                                              << " or no pending request for correlation_id="
                                              << corr_id;
                }
                break;
            }
        }
    } catch (const boost::system::system_error& e) {
        if (e.code() == boost::asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg(), debug) << "Message loop cancelled";
        } else {
            BOOST_LOG_SEV(lg(), error) << "Message loop error: " << e.what();
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Message loop exception: " << e.what();
    }

    // Mark as disconnected and get callback
    disconnect_callback_t callback;
    {
        std::lock_guard guard{state_mutex_};
        connected_ = false;
        callback = disconnect_callback_;
    }

    // Fail all pending requests
    pending_requests_->fail_all(messaging::error_code::network_error);

    // Invoke disconnect callback outside of lock
    if (callback) {
        BOOST_LOG_SEV(lg(), debug) << "Message loop invoking disconnect callback";
        callback();
    }

    message_loop_running_ = false;
    BOOST_LOG_SEV(lg(), debug) << "Message loop ended";
}

}
