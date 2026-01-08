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
#include <memory>
#include <format>
#include <chrono>
#include <random>
#include <thread>
#include <iostream>
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
#include "ores.comms/service/handshake_service.hpp"
#include "ores.comms/messaging/heartbeat_protocol.hpp"
#include "ores.comms/messaging/subscription_protocol.hpp"
#include "ores.comms/eventing/connection_events.hpp"

namespace ores::comms::net {

using namespace ores::telemetry::log;

std::ostream& operator<<(std::ostream& s, connection_state v) {
    switch (v) {
    case connection_state::disconnected: s << "disconnected"; break;
    case connection_state::connecting: s << "connecting"; break;
    case connection_state::connected: s << "connected"; break;
    case connection_state::reconnecting: s << "reconnecting"; break;
    }
    return s;
}

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

    session_compression_ = co_await service::handshake_service::perform_client_handshake(
        *conn_, sequence_generator, config_.client_identifier,
        config_.supported_compression);

    BOOST_LOG_SEV(lg(), info) << "Session compression set to: " << session_compression_;
}

client::client(client_options config,
    std::shared_ptr<ores::eventing::service::event_bus> event_bus)
    : config_(std::move(config)),
      io_ctx_(std::make_unique<boost::asio::io_context>()),
      executor_(io_ctx_->get_executor()),
      ssl_ctx_(boost::asio::ssl::context::tlsv13_client),
      sequence_number_(0), state_(connection_state::disconnected),
      event_bus_(std::move(event_bus)) {
    BOOST_LOG_SEV(lg(), info) << "Client options: " << config_;
    setup_ssl_context();

    // Initialize write strand and pending request map
    write_strand_ = std::make_unique<boost::asio::strand<boost::asio::any_io_executor>>(
        boost::asio::make_strand(executor_));
    pending_requests_ = std::make_unique<pending_request_map>(executor_);
}

client::~client() {
    // Stop recording if active (get copy under lock, then operate outside lock)
    std::shared_ptr<recording::session_recorder> recorder;
    {
        std::lock_guard guard{state_mutex_};
        recorder = recorder_;
    }
    if (recorder) {
        recorder->stop();
    }

    // Ensure disconnect is called to stop any running coroutines
    disconnect();

    // Release work guard to allow io_context to complete when all work is done
    work_guard_.reset();

    // Stop io_context to allow background thread to complete
    if (io_ctx_) {
        io_ctx_->stop();
    }

    // Wait for io_thread to finish
    if (io_thread_ && io_thread_->joinable()) {
        io_thread_->join();
    }
    io_thread_.reset();

    // Explicitly destroy resources that depend on the executor/io_context
    // before the io_context is destroyed. This prevents use-after-free when
    // the strand tries to deallocate through an invalid executor.
    pending_requests_.reset();
    write_strand_.reset();
    conn_.reset();
}

client::client(client_options config, boost::asio::any_io_executor executor,
    std::shared_ptr<ores::eventing::service::event_bus> event_bus)
    : config_(std::move(config)), executor_(std::move(executor)),
      ssl_ctx_(boost::asio::ssl::context::tlsv13_client),
      sequence_number_(0), state_(connection_state::disconnected),
      event_bus_(std::move(event_bus)) {
    BOOST_LOG_SEV(lg(), info) << "Client options: " << config_;
    setup_ssl_context();

    // Initialize write strand and pending request map
    write_strand_ = std::make_unique<boost::asio::strand<boost::asio::any_io_executor>>(
        boost::asio::make_strand(executor_));
    pending_requests_ = std::make_unique<pending_request_map>(executor_);
}

boost::asio::awaitable<void> client::perform_connection() {
    // Early check - if already disconnected, don't even try to connect
    auto current = state_.load(std::memory_order_acquire);
    if (current == connection_state::disconnected) {
        BOOST_LOG_SEV(lg(), info) << "Connection aborted - already disconnected";
        throw connection_error("Connection aborted - already disconnected");
    }

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

    // Check if disconnect was called while we were connecting
    if (state_.load(std::memory_order_acquire) == connection_state::disconnected) {
        BOOST_LOG_SEV(lg(), info) << "Connection aborted after TCP connect - disconnect was called";
        socket.close();
        throw connection_error("Connection aborted by disconnect");
    }

    conn_ = std::make_unique<connection>(
        connection::ssl_socket(std::move(socket), ssl_ctx_));
    co_await conn_->ssl_handshake_client();

    // Check again after SSL handshake
    if (state_.load(std::memory_order_acquire) == connection_state::disconnected) {
        BOOST_LOG_SEV(lg(), info) << "Connection aborted after SSL handshake - disconnect was called";
        if (conn_) {
            conn_->close();
        }
        throw connection_error("Connection aborted by disconnect");
    }

    BOOST_LOG_SEV(lg(), debug) << "SSL handshake complete.";

    BOOST_LOG_SEV(lg(), debug) << "Protocol version: "
                               << messaging::PROTOCOL_VERSION_MAJOR << "."
                               << messaging::PROTOCOL_VERSION_MINOR
                               << " (client: " << config_.client_identifier << ")";

    co_await perform_handshake();

    // Only transition to connected if we're in an expected state.
    // If disconnect() was called during connection, state will be disconnected
    // and we should abort rather than overwrite that state.
    auto current_state = state_.load(std::memory_order_acquire);
    if (current_state == connection_state::disconnected) {
        BOOST_LOG_SEV(lg(), info) << "Connection aborted - disconnect was called during handshake";
        if (conn_) {
            conn_->close();
        }
        throw connection_error("Connection aborted by disconnect");
    }

    // Try to transition from connecting/reconnecting to connected
    auto expected = connection_state::connecting;
    if (!state_.compare_exchange_strong(expected, connection_state::connected,
            std::memory_order_acq_rel)) {
        expected = connection_state::reconnecting;
        if (!state_.compare_exchange_strong(expected, connection_state::connected,
                std::memory_order_acq_rel)) {
            BOOST_LOG_SEV(lg(), info) << "Connection completed but state changed to "
                                      << static_cast<int>(expected) << ", closing connection";
            if (conn_) {
                conn_->close();
            }
            throw connection_error("Connection aborted - state changed during connection");
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully connected to server.";

    // Publish connected event to event bus
    if (event_bus_) {
        event_bus_->publish(comms::eventing::connected_event{
            .timestamp = std::chrono::system_clock::now(),
            .host = config_.host,
            .port = config_.port
        });
    }

    // Start message loop in background (single reader for all responses)
    // Only needed when using correlation-based request/response or heartbeat
    if (config_.heartbeat_enabled && !message_loop_running_) {
        boost::asio::co_spawn(exec, run_message_loop(), boost::asio::detached);
        BOOST_LOG_SEV(lg(), debug) << "Message loop started";
    }

    // Start heartbeat in background (only if not already running)
    if (config_.heartbeat_enabled && !heartbeat_loop_running_) {
        boost::asio::co_spawn(exec, run_heartbeat(), boost::asio::detached);
        BOOST_LOG_SEV(lg(), info) << "Heartbeat enabled with interval: "
                                   << config_.heartbeat_interval_seconds << "s";
    } else if (!config_.heartbeat_enabled) {
        BOOST_LOG_SEV(lg(), info) << "Heartbeat disabled";
    }
}

boost::asio::awaitable<void> client::connect() {
    auto expected = connection_state::disconnected;
    if (!state_.compare_exchange_strong(expected, connection_state::connecting,
            std::memory_order_acq_rel)) {
        throw connection_error("Client is already connected or connecting");
    }

    try {
        co_await perform_connection();
    } catch (const connection_error&) {
        state_.store(connection_state::disconnected, std::memory_order_release);
        throw;
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Connection error: " << e.what();
        state_.store(connection_state::disconnected, std::memory_order_release);
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

    // Start io_context in background thread if we own it and it's not already running.
    // The work guard keeps io_context alive for subsequent sync operations.
    if (io_ctx_ && !io_thread_) {
        BOOST_LOG_SEV(lg(), trace) << "connect_sync: creating work guard and starting io_context thread.";
        work_guard_ = std::make_unique<
            boost::asio::executor_work_guard<boost::asio::io_context::executor_type>>(
                io_ctx_->get_executor());
        io_thread_ = std::make_unique<std::thread>([this]() {
            BOOST_LOG_SEV(lg(), trace) << "io_thread: starting io_context.run().";
            io_ctx_->run();
            BOOST_LOG_SEV(lg(), trace) << "io_thread: io_context.run() completed.";
        });
    }

    BOOST_LOG_SEV(lg(), debug) << "connect_sync: getting result from future.";
    future.get();
    BOOST_LOG_SEV(lg(), debug) << "Completed synchronous connect successfully.";
}

std::chrono::milliseconds client::calculate_backoff(std::uint32_t attempt) const {
    using namespace std::chrono;

    // Exponential backoff: base * 2^attempt
    auto delay = config_.retry.base_delay * (1 << std::min(attempt, 20u));

    // Cap at max delay
    if (delay > config_.retry.max_delay) {
        delay = config_.retry.max_delay;
    }

    // Apply jitter (±jitter_factor)
    if (config_.retry.jitter_factor > 0.0) {
        // Use robust seed combining multiple entropy sources to avoid
        // synchronized retry intervals on platforms with weak random_device
        static thread_local std::mt19937 rng = []() {
            std::random_device rd;
            auto seed = static_cast<std::uint64_t>(rd()) ^
                static_cast<std::uint64_t>(
                    std::chrono::high_resolution_clock::now().time_since_epoch().count()) ^
                static_cast<std::uint64_t>(
                    std::hash<std::thread::id>{}(std::this_thread::get_id()));
            return std::mt19937(static_cast<std::uint32_t>(seed));
        }();
        std::uniform_real_distribution<double> dist(
            1.0 - config_.retry.jitter_factor,
            1.0 + config_.retry.jitter_factor);
        delay = duration_cast<milliseconds>(delay * dist(rng));
    }

    return delay;
}

boost::asio::awaitable<void> client::connect_with_retry() {
    const auto max_attempts = config_.retry.max_attempts;
    if (max_attempts == 0) {
        // No retries configured, just do single attempt
        co_await connect();
        co_return;
    }

    auto expected = connection_state::disconnected;
    if (!state_.compare_exchange_strong(expected, connection_state::connecting,
            std::memory_order_acq_rel)) {
        throw connection_error("Client is already connected or connecting");
    }

    auto exec = co_await boost::asio::this_coro::executor;
    boost::asio::steady_timer timer(exec);
    std::exception_ptr last_exception;

    for (std::uint32_t attempt = 0; attempt < max_attempts; ++attempt) {
        bool should_retry = false;
        std::chrono::milliseconds retry_delay{0};

        try {
            BOOST_LOG_SEV(lg(), info) << "Connection attempt " << (attempt + 1)
                                      << " of " << max_attempts;
            co_await perform_connection();
            co_return;  // Success
        } catch (const std::exception& e) {
            last_exception = std::current_exception();
            BOOST_LOG_SEV(lg(), warn) << "Connection attempt " << (attempt + 1)
                                      << " failed: " << e.what();

            // Don't wait after the last attempt
            if (attempt + 1 < max_attempts) {
                retry_delay = calculate_backoff(attempt);
                should_retry = true;
            }
        }

        // Wait outside catch block (co_await cannot be in catch handler)
        if (should_retry) {
            BOOST_LOG_SEV(lg(), info) << "Retrying in " << retry_delay.count() << "ms";
            timer.expires_after(retry_delay);
            co_await timer.async_wait(boost::asio::use_awaitable);
        }
    }

    // All attempts failed
    state_.store(connection_state::disconnected, std::memory_order_release);

    BOOST_LOG_SEV(lg(), error) << "All " << max_attempts
                               << " connection attempts failed";
    if (last_exception) {
        std::rethrow_exception(last_exception);
    }
    throw connection_error("All connection attempts failed");
}

void client::connect_with_retry_sync() {
    BOOST_LOG_SEV(lg(), debug) << "Starting to connect with retry synchronously.";

    auto task = [this]() -> boost::asio::awaitable<void> {
        BOOST_LOG_SEV(lg(), trace) << "connect_with_retry_sync: task started.";
        co_await connect_with_retry();
        BOOST_LOG_SEV(lg(), trace) << "connect_with_retry_sync: task completed.";
    };

    BOOST_LOG_SEV(lg(), trace) << "connect_with_retry_sync: spawning task with use_future.";
    auto future = boost::asio::co_spawn(executor_, task(), boost::asio::use_future);

    // Start io_context in background thread if we own it and it's not already running.
    // The work guard keeps io_context alive for subsequent sync operations.
    if (io_ctx_ && !io_thread_) {
        BOOST_LOG_SEV(lg(), trace) << "connect_with_retry_sync: creating work guard and starting io_context thread.";
        work_guard_ = std::make_unique<
            boost::asio::executor_work_guard<boost::asio::io_context::executor_type>>(
                io_ctx_->get_executor());
        io_thread_ = std::make_unique<std::thread>([this]() {
            BOOST_LOG_SEV(lg(), trace) << "io_thread: starting io_context.run().";
            io_ctx_->run();
            BOOST_LOG_SEV(lg(), trace) << "io_thread: io_context.run() completed.";
        });
    }

    BOOST_LOG_SEV(lg(), debug) << "connect_with_retry_sync: getting result from future.";
    future.get();
    BOOST_LOG_SEV(lg(), debug) << "Completed synchronous connect with retry successfully.";
}

boost::asio::awaitable<void> client::run_reconnect_loop() {
    bool expected = false;
    if (!reconnect_loop_running_.compare_exchange_strong(expected, true)) {
        BOOST_LOG_SEV(lg(), debug) << "Reconnect loop already running";
        co_return;
    }

    BOOST_LOG_SEV(lg(), info) << "Starting reconnection loop";

    const auto max_attempts = config_.retry.max_attempts;
    auto exec = co_await boost::asio::this_coro::executor;
    boost::asio::steady_timer timer(exec);

    for (std::uint32_t attempt = 0; attempt < max_attempts; ++attempt) {
        // Check if we should still try to reconnect
        if (state_.load(std::memory_order_acquire) != connection_state::reconnecting) {
            BOOST_LOG_SEV(lg(), debug) << "Reconnect cancelled, state changed";
            break;
        }

        bool should_retry = false;
        std::chrono::milliseconds retry_delay{0};

        const bool is_final_attempt = (attempt + 1 == max_attempts);
        try {
            if (is_final_attempt) {
                BOOST_LOG_SEV(lg(), warn) << "Final reconnection attempt " << (attempt + 1)
                                          << " of " << max_attempts;
            } else {
                BOOST_LOG_SEV(lg(), info) << "Reconnection attempt " << (attempt + 1)
                                          << " of " << max_attempts;
            }
            co_await perform_connection();

            // Check state again after connection completes - disconnect() may have
            // been called while we were connecting
            if (state_.load(std::memory_order_acquire) != connection_state::reconnecting) {
                BOOST_LOG_SEV(lg(), info) << "Reconnect cancelled after connection established, "
                                          << "closing new connection";
                {
                    std::lock_guard guard{state_mutex_};
                    if (conn_) {
                        conn_->close();
                    }
                }
                reconnect_loop_running_ = false;
                co_return;
            }

            BOOST_LOG_SEV(lg(), info) << "Reconnection successful";

            // Publish reconnected event to event bus
            if (event_bus_) {
                event_bus_->publish(comms::eventing::reconnected_event{
                    .timestamp = std::chrono::system_clock::now()
                });
            }

            // Invoke reconnected callback to notify UI
            reconnected_callback_t reconnected_cb;
            {
                std::lock_guard guard{state_mutex_};
                reconnected_cb = reconnected_callback_;
            }
            if (reconnected_cb) {
                BOOST_LOG_SEV(lg(), debug) << "Invoking reconnected callback";
                reconnected_cb();
            }

            reconnect_loop_running_ = false;
            co_return;
        } catch (const connection_error& e) {
            // Check if this was an intentional abort (user disconnect)
            std::string_view msg = e.what();
            if (msg.find("aborted") != std::string_view::npos) {
                BOOST_LOG_SEV(lg(), info) << "Reconnection attempt " << (attempt + 1)
                                          << " cancelled: " << e.what();
                // Don't retry if aborted
                break;
            }

            BOOST_LOG_SEV(lg(), warn) << "Reconnection attempt " << (attempt + 1)
                                      << " failed: " << e.what();

            // Don't wait after the last attempt
            if (attempt + 1 < max_attempts) {
                retry_delay = calculate_backoff(attempt);
                should_retry = true;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Reconnection attempt " << (attempt + 1)
                                      << " failed: " << e.what();

            // Don't wait after the last attempt
            if (attempt + 1 < max_attempts) {
                retry_delay = calculate_backoff(attempt);
                should_retry = true;
            }
        }

        // Wait outside catch block (co_await cannot be in catch handler)
        if (should_retry) {
            BOOST_LOG_SEV(lg(), info) << "Retrying reconnection in "
                                      << retry_delay.count() << "ms";
            timer.expires_after(retry_delay);

            try {
                co_await timer.async_wait(boost::asio::use_awaitable);
            } catch (const boost::system::system_error& te) {
                if (te.code() == boost::asio::error::operation_aborted) {
                    BOOST_LOG_SEV(lg(), debug) << "Reconnect timer cancelled";
                    break;
                }
                throw;
            }
        }
    }

    // Check why we exited the loop
    auto final_state = state_.load(std::memory_order_acquire);
    if (final_state == connection_state::disconnected) {
        // User called disconnect() - this is an intentional cancellation, not a failure
        BOOST_LOG_SEV(lg(), info) << "Reconnection cancelled by user disconnect";
        reconnect_loop_running_ = false;
        co_return;
    }

    // All reconnection attempts failed - give up
    BOOST_LOG_SEV(lg(), error) << "All " << max_attempts
                               << " reconnection attempts failed. "
                               << "Giving up, auto-reconnect will not retry.";

    state_.store(connection_state::disconnected, std::memory_order_release);

    disconnect_callback_t callback;
    {
        std::lock_guard guard{state_mutex_};
        callback = disconnect_callback_;
    }

    // Publish disconnected event to event bus
    if (event_bus_) {
        event_bus_->publish(comms::eventing::disconnected_event{
            .timestamp = std::chrono::system_clock::now()
        });
    }

    // Invoke disconnect callback only after all retries exhausted
    if (callback) {
        BOOST_LOG_SEV(lg(), debug) << "Invoking disconnect callback after reconnect failure";
        callback();
    }

    reconnect_loop_running_ = false;
}

void client::disconnect() {
    BOOST_LOG_SEV(lg(), debug) << "Disconnecting from server";

    // First mark as disconnected to stop the message loop and reconnect loop
    state_.store(connection_state::disconnected, std::memory_order_release);

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
        pending_requests_->fail_all(ores::utility::serialization::error_code::network_error);
    }

    BOOST_LOG_SEV(lg(), info) << "Disconnected from server";
}

bool client::is_connected() const {
    return state_.load(std::memory_order_acquire) == connection_state::connected;
}

connection_state client::get_state() const {
    return state_.load(std::memory_order_acquire);
}

void client::set_disconnect_callback(disconnect_callback_t callback) {
    std::lock_guard guard{state_mutex_};
    disconnect_callback_ = std::move(callback);
}

void client::set_reconnecting_callback(reconnecting_callback_t callback) {
    std::lock_guard guard{state_mutex_};
    reconnecting_callback_ = std::move(callback);
}

void client::set_reconnected_callback(reconnected_callback_t callback) {
    std::lock_guard guard{state_mutex_};
    reconnected_callback_ = std::move(callback);
}

void client::set_notification_callback(notification_callback_t callback) {
    std::lock_guard guard{state_mutex_};
    notification_callback_ = std::move(callback);
}

std::expected<std::filesystem::path, recording::session_file_error>
client::enable_recording(const std::filesystem::path& output_directory) {
    // Create new recorder or reuse existing one (thread-safe initialization under mutex)
    std::shared_ptr<recording::session_recorder> recorder;
    {
        std::lock_guard guard{state_mutex_};
        if (!recorder_) {
            recorder_ = std::make_shared<recording::session_recorder>();
        }
        recorder = recorder_;
    }

    // Get server address for the recording header
    std::string server_address = config_.host + ":" + std::to_string(config_.port);

    auto result = recorder->start(output_directory, server_address, session_compression_);
    if (result) {
        BOOST_LOG_SEV(lg(), info) << "Session recording enabled: " << *result;
    } else {
        BOOST_LOG_SEV(lg(), error) << "Failed to enable session recording";
    }

    return result;
}

void client::disable_recording() {
    std::shared_ptr<recording::session_recorder> recorder;
    {
        std::lock_guard guard{state_mutex_};
        recorder = recorder_;
    }
    if (recorder) {
        recorder->stop();
        BOOST_LOG_SEV(lg(), info) << "Session recording disabled";
    }
}

bool client::is_recording() const {
    std::shared_ptr<recording::session_recorder> recorder;
    {
        std::lock_guard guard{state_mutex_};
        recorder = recorder_;
    }
    return recorder && recorder->is_recording();
}

boost::asio::awaitable<void> client::run_heartbeat() {
    if (!config_.heartbeat_enabled) {
        BOOST_LOG_SEV(lg(), debug) << "Heartbeat disabled in configuration";
        co_return;
    }

    bool expected = false;
    if (!heartbeat_loop_running_.compare_exchange_strong(expected, true)) {
        BOOST_LOG_SEV(lg(), debug) << "Heartbeat loop already running";
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
                if (!conn_ || state_.load(std::memory_order_acquire) != connection_state::connected) {
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

    heartbeat_loop_running_ = false;
    BOOST_LOG_SEV(lg(), debug) << "Heartbeat loop ended";
}

boost::asio::awaitable<std::expected<messaging::frame, ores::utility::serialization::error_code>>
client::send_request(messaging::frame request_frame) {
    BOOST_LOG_SEV(lg(), debug) << "Sending request.";

    // Check connection state - fail immediately if not connected
    auto current_state = state_.load(std::memory_order_acquire);
    if (current_state == connection_state::reconnecting) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot send request: reconnecting";
        co_return std::unexpected(ores::utility::serialization::error_code::network_error);
    }
    if (current_state != connection_state::connected) {
        BOOST_LOG_SEV(lg(), error) << "Cannot send request: not connected (state="
                                   << current_state << ")";
        co_return std::unexpected(ores::utility::serialization::error_code::network_error);
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

        // Create frame with correlation ID and session compression
        auto frame_to_send = messaging::frame(
            request_frame.header().type, seq, corr_id,
            std::vector<std::byte>(request_frame.payload()),
            session_compression_);

        BOOST_LOG_SEV(lg(), debug) << "Sending request " << frame_to_send.header().type
                                   << " (correlation_id=" << corr_id << ")";

        try {
            co_await write_frame(frame_to_send);
        } catch (const std::exception& e) {
            pending_requests_->remove(corr_id);
            BOOST_LOG_SEV(lg(), error) << "Failed to send request: " << e.what();
            co_return std::unexpected(ores::utility::serialization::error_code::network_error);
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
            std::vector<std::byte>(request_frame.payload()),
            session_compression_);

        BOOST_LOG_SEV(lg(), debug) << "Sending request (direct) "
                                   << frame_to_send.header().type;

        co_await conn_->write_frame(frame_to_send);

        // Record the sent frame if recording is active (direct path)
        std::shared_ptr<recording::session_recorder> sent_recorder;
        {
            std::lock_guard guard{state_mutex_};
            sent_recorder = recorder_;
        }
        if (sent_recorder && sent_recorder->is_recording()) {
            sent_recorder->record_sent(frame_to_send);
        }

        auto response_result = co_await conn_->read_frame();
        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to read response frame, error "
                                       << response_result.error();
            state_.store(connection_state::disconnected, std::memory_order_release);
            {
                std::lock_guard guard{state_mutex_};
                if (conn_) {
                    conn_->close();
                }
            }
            BOOST_LOG_SEV(lg(), warn) << "Connection lost";
            co_return std::unexpected(response_result.error());
        }

        // Record the received frame if recording is active (direct path)
        std::shared_ptr<recording::session_recorder> recv_recorder;
        {
            std::lock_guard guard{state_mutex_};
            recv_recorder = recorder_;
        }
        if (recv_recorder && recv_recorder->is_recording()) {
            recv_recorder->record_received(*response_result);
        }

        BOOST_LOG_SEV(lg(), debug) << "Received response "
                                   << response_result->header().type;
        co_return *response_result;

    } catch (const boost::system::system_error& e) {
        BOOST_LOG_SEV(lg(), error) << "Network error: " << e.what();
        state_.store(connection_state::disconnected, std::memory_order_release);
        {
            std::lock_guard guard{state_mutex_};
            if (conn_) {
                conn_->close();
            }
        }
        co_return std::unexpected(ores::utility::serialization::error_code::network_error);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Request exception: " << e.what();
        co_return std::unexpected(ores::utility::serialization::error_code::network_error);
    }
}

std::expected<messaging::frame, ores::utility::serialization::error_code>
client::send_request_sync(messaging::frame request_frame) {
    BOOST_LOG_SEV(lg(), debug) << "Starting to send request synchronously.";

    using result_type = std::expected<messaging::frame, ores::utility::serialization::error_code>;

    auto task = [this, request_frame = std::move(
            request_frame)]() mutable -> boost::asio::awaitable<result_type> {
        BOOST_LOG_SEV(lg(), trace) << "send_request_sync: task started.";
        result_type result = co_await send_request(std::move(request_frame));
        BOOST_LOG_SEV(lg(), trace) << "send_request_sync: task completed.";
        co_return result;
    };

    BOOST_LOG_SEV(lg(), trace) << "send_request_sync: spawning task with use_future";
    auto future = boost::asio::co_spawn(executor_, task(), boost::asio::use_future);

    // io_context is already running in background thread (started by connect_sync)
    // Just wait for the future to complete.
    BOOST_LOG_SEV(lg(), debug) << "send_request_sync: getting result from future.";
    auto r = future.get();
    BOOST_LOG_SEV(lg(), debug) << "Completed synchronous send request successfully.";
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

    // Record the sent frame if recording is active
    std::shared_ptr<recording::session_recorder> recorder;
    {
        std::lock_guard guard{state_mutex_};
        recorder = recorder_;
    }
    if (recorder && recorder->is_recording()) {
        recorder->record_sent(f);
    }
}

boost::asio::awaitable<void> client::run_message_loop() {
    bool expected = false;
    if (!message_loop_running_.compare_exchange_strong(expected, true)) {
        BOOST_LOG_SEV(lg(), debug) << "Message loop already running";
        co_return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Starting message loop";

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

            // Record the received frame if recording is active
            std::shared_ptr<recording::session_recorder> recorder;
            {
                std::lock_guard guard{state_mutex_};
                recorder = recorder_;
            }
            if (recorder && recorder->is_recording()) {
                recorder->record_received(frame);
            }

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

            case messaging::message_type::notification: {
                // Server-push notification - invoke callback if registered
                BOOST_LOG_SEV(lg(), debug) << "Received notification from server";

                auto notification_result = messaging::notification_message::deserialize(
                    frame.payload());

                if (notification_result) {
                    notification_callback_t callback;
                    {
                        std::lock_guard guard{state_mutex_};
                        callback = notification_callback_;
                    }

                    if (callback) {
                        BOOST_LOG_SEV(lg(), debug) << "Invoking notification callback for "
                                                   << notification_result->event_type
                                                   << " with " << notification_result->entity_ids.size()
                                                   << " entity IDs";
                        callback(notification_result->event_type,
                                 notification_result->timestamp,
                                 notification_result->entity_ids);
                    } else {
                        BOOST_LOG_SEV(lg(), trace) << "No notification callback registered, "
                                                   << "ignoring notification for "
                                                   << notification_result->event_type;
                    }
                } else {
                    BOOST_LOG_SEV(lg(), warn) << "Failed to deserialize notification message";
                }
                break;
            }

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

    // Fail all pending requests so waiting coroutines don't hang
    if (pending_requests_) {
        pending_requests_->fail_all(ores::utility::serialization::error_code::network_error);
    }

    message_loop_running_ = false;

    // Check if we should attempt auto-reconnect
    bool should_reconnect = false;
    reconnecting_callback_t reconnecting_cb;

    // Only auto-reconnect if we were connected (not explicitly disconnected)
    if (config_.retry.auto_reconnect && config_.retry.max_attempts > 0) {
        auto expected = connection_state::connected;
        if (state_.compare_exchange_strong(expected, connection_state::reconnecting,
                std::memory_order_acq_rel)) {
            should_reconnect = true;
            {
                std::lock_guard guard{state_mutex_};
                reconnecting_cb = reconnecting_callback_;
            }
            BOOST_LOG_SEV(lg(), info) << "Connection lost, will attempt auto-reconnect";
        }
    }

    // If not reconnecting and not already disconnected, mark as disconnected
    if (!should_reconnect) {
        auto expected = connection_state::connected;
        state_.compare_exchange_strong(expected, connection_state::disconnected,
            std::memory_order_acq_rel);
    }

    if (should_reconnect) {
        // Publish reconnecting event to event bus
        if (event_bus_) {
            event_bus_->publish(comms::eventing::reconnecting_event{
                .timestamp = std::chrono::system_clock::now()
            });
        }

        // Invoke reconnecting callback to notify UI
        if (reconnecting_cb) {
            BOOST_LOG_SEV(lg(), debug) << "Invoking reconnecting callback";
            reconnecting_cb();
        }
        // Spawn reconnect loop - it will invoke disconnect callback if all retries fail
        auto exec = co_await boost::asio::this_coro::executor;
        boost::asio::co_spawn(exec, run_reconnect_loop(), boost::asio::detached);
    } else {
        // Publish disconnected event to event bus
        if (event_bus_) {
            event_bus_->publish(comms::eventing::disconnected_event{
                .timestamp = std::chrono::system_clock::now()
            });
        }

        // No auto-reconnect, invoke callback immediately if set
        disconnect_callback_t callback;
        {
            std::lock_guard guard{state_mutex_};
            callback = disconnect_callback_;
        }
        if (callback) {
            BOOST_LOG_SEV(lg(), debug) << "Message loop invoking disconnect callback";
            callback();
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Message loop ended";
}

}
