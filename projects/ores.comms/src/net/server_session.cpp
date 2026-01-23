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
#include "ores.comms/net/server_session.hpp"

#include <boost/asio/redirect_error.hpp>
#include <boost/asio/experimental/awaitable_operators.hpp>
#include "ores.comms/messaging/handshake_protocol.hpp"
#include "ores.comms/messaging/subscription_protocol.hpp"
#include "ores.comms/service/handshake_service.hpp"
#include "ores.comms/service/heartbeat_service.hpp"
#include "ores.comms/service/subscription_manager.hpp"

using namespace boost::asio::experimental::awaitable_operators;

namespace ores::comms::net {

using namespace ores::logging;

server_session::server_session(std::unique_ptr<connection> conn, std::string server_id,
    std::shared_ptr<messaging::message_dispatcher> dispatcher,
    boost::asio::any_io_executor io_executor,
    std::shared_ptr<service::auth_session_service> sessions,
    std::shared_ptr<service::subscription_manager> subscription_mgr)
    : conn_(std::move(conn)),
      server_id_(std::move(server_id)),
      dispatcher_(std::move(dispatcher)),
      sessions_(std::move(sessions)),
      subscription_mgr_(std::move(subscription_mgr)),
      sequence_number_(0),
      handshake_complete_(false),
      write_strand_(boost::asio::make_strand(io_executor)),
      notification_signal_(io_executor) {
    // Set timer to "never expire" initially - it will be cancelled to signal
    notification_signal_.expires_at(std::chrono::steady_clock::time_point::max());
}

void server_session::stop() {
    BOOST_LOG_SEV(lg(), info) << "Stopping session for " << conn_->remote_address();
    active_ = false;

    // Cancel notification signal to wake up the notification writer coroutine
    // so it can exit gracefully before we close the connection
    notification_signal_.cancel();

    if (conn_) {
        conn_->close();
    }
}

bool server_session::queue_notification(const std::string& event_type,
    std::chrono::system_clock::time_point timestamp,
    const std::vector<std::string>& entity_ids) {
    if (!active_) {
        BOOST_LOG_SEV(lg(), debug)
            << "Cannot queue notification - session not active";
        return false;
    }

    {
        std::lock_guard lock(notification_mutex_);
        pending_notifications_.push({event_type, timestamp, entity_ids});
    }

    // Signal the notification writer by cancelling the timer
    // This will wake up the waiting coroutine immediately
    notification_signal_.cancel();

    BOOST_LOG_SEV(lg(), debug)
        << "Queued notification for event type '" << event_type
        << "' with " << entity_ids.size() << " entity IDs";
    return true;
}

bool server_session::queue_database_status(bool available, const std::string& error_message,
    std::chrono::system_clock::time_point timestamp) {
    if (!active_) {
        BOOST_LOG_SEV(lg(), debug)
            << "Cannot queue database status - session not active";
        return false;
    }

    {
        std::lock_guard lock(notification_mutex_);
        pending_database_status_.push({available, error_message, timestamp});
    }

    // Signal the notification writer by cancelling the timer
    notification_signal_.cancel();

    BOOST_LOG_SEV(lg(), debug)
        << "Queued database status notification (available=" << available << ")";
    return true;
}

boost::asio::awaitable<void> server_session::run() {
    std::string remote_addr = conn_->remote_address();
    try {
        BOOST_LOG_SEV(lg(), info) << "Session started for client: " << remote_addr;

        // Perform SSL handshake with cancellation support
        co_await conn_->ssl_handshake_server();

        // Perform protocol handshake
        bool handshake_ok = co_await perform_handshake();
        if (!handshake_ok) {
            BOOST_LOG_SEV(lg(), warn) << "Handshake failed for client: " << remote_addr;
            co_return;
        }

        BOOST_LOG_SEV(lg(), info) << "Handshake complete for client: " << remote_addr;

        // Mark session as active and register with subscription manager
        active_ = true;
        register_with_subscription_manager();

        // Run message processing and notification writer concurrently.
        // When either completes (e.g., client disconnects), both will stop.
        co_await (process_messages() || run_notification_writer());

    } catch (const boost::system::system_error& e) {
        if (e.code() == boost::asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg(), info) << "Session cancelled for " << remote_addr;
        } else {
            BOOST_LOG_SEV(lg(), error) << "Session error for " << remote_addr << ": " << e.what();
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Session error for " << remote_addr << ": " << e.what();
    }

    // Mark as inactive and unregister
    active_ = false;
    unregister_from_subscription_manager();

    // Cancel the notification signal to ensure writer coroutine exits
    notification_signal_.cancel();

    conn_->close();
    BOOST_LOG_SEV(lg(), info) << "Session ended for client: " << remote_addr;
}

boost::asio::awaitable<bool> server_session::perform_handshake() {
    auto handshake_result = co_await service::handshake_service::perform_server_handshake(
        *conn_, ++sequence_number_, server_id_);

    if (handshake_result) {
        session_compression_ = handshake_result->compression;
        handshake_complete_ = true;
        BOOST_LOG_SEV(lg(), info) << "Session compression set to: " << session_compression_;

        // Store client info from handshake for later use during login
        if (sessions_) {
            service::client_info info{
                .client_identifier = handshake_result->client_identifier,
                .client_version_major = handshake_result->client_version_major,
                .client_version_minor = handshake_result->client_version_minor
            };
            sessions_->store_client_info(conn_->remote_address(), std::move(info));
        }

        co_return true;
    }

    co_return false;
}

boost::asio::awaitable<void> server_session::process_messages() {
    BOOST_LOG_SEV(lg(), debug) << "Starting message processing loop";

    try {
        while (active_) {
            // Read next message frame
            auto read_result = co_await conn_->read_frame(false);
            if (!read_result.frame) {
                auto err = read_result.frame.error();

                // For CRC errors, close the session - the stream may be corrupted.
                // CRC errors indicate data corruption which may have affected the
                // payload_size header field. If so, the stream is now out of sync
                // and subsequent reads will fail with invalid data.
                if (err == ores::utility::serialization::error_code::crc_validation_failed) {
                    BOOST_LOG_SEV(lg(), warn) << "CRC validation failed"
                                             << (read_result.failed_correlation_id
                                                 ? " for correlation_id=" + std::to_string(*read_result.failed_correlation_id)
                                                 : "")
                                             << ", closing session (stream may be corrupted)";
                    co_return;
                }

                if (err == ores::utility::serialization::error_code::network_error) {
                    BOOST_LOG_SEV(lg(), info) << "Client disconnected";
                } else {
                    BOOST_LOG_SEV(lg(), error) << "Failed to read frame: "
                                              << static_cast<int>(err);
                }
                co_return;
            }

            const auto& request_frame = *read_result.frame;
            BOOST_LOG_SEV(lg(), debug) << "Received message type "
                                      << request_frame.header().type;

            // Handle ping messages directly (built-in protocol feature)
            if (request_frame.header().type == messaging::message_type::ping) {
                co_await service::heartbeat_service::handle_ping(
                    *conn_, ++sequence_number_, request_frame.correlation_id());
                continue;  // Don't send additional response
            }

            // Dispatch to appropriate handler
            auto remote_addr = conn_->remote_address();
            messaging::frame response_frame{messaging::message_type::error_response, 0, {}};

            try {
                auto response_result = co_await dispatcher_->dispatch(request_frame,
                    ++sequence_number_, remote_addr, session_compression_);

                if (!response_result) {
                    auto err = response_result.error();
                    BOOST_LOG_SEV(lg(), error) << "Message dispatch failed: "
                                              << static_cast<int>(err);

                    // Create error response with appropriate message
                    std::string error_msg;
                    switch (err) {
                        case ores::utility::serialization::error_code::invalid_message_type:
                            error_msg = "Invalid or unsupported message type";
                            break;
                        case ores::utility::serialization::error_code::handler_error:
                            error_msg = "Request handler encountered an error";
                            break;
                        case ores::utility::serialization::error_code::database_error:
                            error_msg = "Database operation failed";
                            break;
                        case ores::utility::serialization::error_code::authentication_failed:
                            error_msg = "Authentication failed";
                            break;
                        case ores::utility::serialization::error_code::authorization_failed:
                            error_msg = "Authorization failed";
                            break;
                        case ores::utility::serialization::error_code::invalid_request:
                            error_msg = "Invalid request parameters";
                            break;
                        case ores::utility::serialization::error_code::bootstrap_mode_only:
                            error_msg = "System is in bootstrap mode. Only initial admin account creation is allowed. Please create the initial admin account from localhost.";
                            break;
                        case ores::utility::serialization::error_code::bootstrap_mode_forbidden:
                            error_msg = "Operation not allowed - system is not in bootstrap mode";
                            break;
                        case ores::utility::serialization::error_code::weak_password:
                            error_msg = "Password does not meet security requirements";
                            break;
                        case ores::utility::serialization::error_code::not_localhost:
                            error_msg = "Bootstrap operations are only allowed from localhost";
                            break;
                        case ores::utility::serialization::error_code::decompression_failed:
                            error_msg = "Failed to decompress request payload - possible compression mismatch";
                            break;
                        case ores::utility::serialization::error_code::unsupported_compression:
                            error_msg = "Server does not support the compression algorithm used in request";
                            break;
                        case ores::utility::serialization::error_code::compression_failed:
                            error_msg = "Failed to compress response payload";
                            break;
                        case ores::utility::serialization::error_code::limit_exceeded:
                            error_msg = "Request limit exceeds maximum allowed (1000)";
                            break;
                        default:
                            error_msg = "An error occurred processing your request";
                            break;
                    }

                    response_frame = messaging::create_error_response_frame(
                        sequence_number_, request_frame.correlation_id(), err, error_msg);
                    BOOST_LOG_SEV(lg(), debug) << "Sending error response: " << error_msg
                                              << " (correlation_id=" << request_frame.correlation_id() << ")";
                } else {
                    response_frame = *response_result;
                }
            } catch (const std::exception& e) {
                // Handler threw an exception - send detailed error to client
                BOOST_LOG_SEV(lg(), error) << "Exception in message handler: " << e.what();
                response_frame = messaging::create_error_response_frame(
                    sequence_number_, request_frame.correlation_id(),
                    ores::utility::serialization::error_code::database_error, e.what());
                BOOST_LOG_SEV(lg(), debug) << "Sending error response with exception details"
                                          << " (correlation_id=" << request_frame.correlation_id() << ")";
            }

            // Send response back to client
            co_await write_frame_serialized(response_frame);
            BOOST_LOG_SEV(lg(), debug) << "Sent response for message type "
                                      << request_frame.header().type;

            // Close connection after logout
            if (request_frame.header().type == messaging::message_type::logout_request) {
                BOOST_LOG_SEV(lg(), info) << "Logout completed, closing connection";
                active_ = false;
                notification_signal_.cancel();
                co_return;
            }
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Exception in message processing: " << e.what();
    }
}

boost::asio::awaitable<void> server_session::run_notification_writer() {
    BOOST_LOG_SEV(lg(), debug) << "Starting notification writer coroutine";

    try {
        while (active_) {
            // Wait for notification signal (timer cancellation)
            boost::system::error_code ec;
            co_await notification_signal_.async_wait(
                boost::asio::redirect_error(boost::asio::use_awaitable, ec));

            // Check if we should exit
            if (!active_) {
                break;
            }

            // Timer was cancelled (signalled) or expired - send pending notifications
            if (ec == boost::asio::error::operation_aborted) {
                // Reset timer for next wait
                notification_signal_.expires_at(
                    std::chrono::steady_clock::time_point::max());

                // Send all pending notifications
                co_await send_pending_notifications();

                // Send all pending database status notifications
                co_await send_pending_database_status();
            }
        }
    } catch (const std::exception& e) {
        // Operation canceled is expected during session shutdown
        if (active_) {
            BOOST_LOG_SEV(lg(), error)
                << "Exception in notification writer: " << e.what();
        } else {
            BOOST_LOG_SEV(lg(), debug)
                << "Notification writer interrupted during shutdown: " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Notification writer coroutine ended";
}

boost::asio::awaitable<void> server_session::send_pending_notifications() {
    std::vector<pending_notification> to_send;

    {
        std::lock_guard lock(notification_mutex_);
        while (!pending_notifications_.empty()) {
            to_send.push_back(std::move(pending_notifications_.front()));
            pending_notifications_.pop();
        }
    }

    if (to_send.empty()) {
        co_return;
    }

    BOOST_LOG_SEV(lg(), debug)
        << "Sending " << to_send.size() << " pending notification(s)";

    for (const auto& notification : to_send) {
        try {
            messaging::notification_message msg{
                .event_type = notification.event_type,
                .timestamp = notification.timestamp,
                .entity_ids = notification.entity_ids
            };

            auto payload = msg.serialize();
            messaging::frame frame{
                messaging::message_type::notification,
                ++sequence_number_,
                std::move(payload)
            };

            co_await write_frame_serialized(frame);

            BOOST_LOG_SEV(lg(), info)
                << "Sent notification for event type '" << notification.event_type
                << "' with " << notification.entity_ids.size() << " entity IDs"
                << " to " << conn_->remote_address();

        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to send notification for event type '"
                << notification.event_type << "': " << e.what();
        }
    }
}

boost::asio::awaitable<void> server_session::send_pending_database_status() {
    std::vector<pending_database_status> to_send;

    {
        std::lock_guard lock(notification_mutex_);
        while (!pending_database_status_.empty()) {
            to_send.push_back(std::move(pending_database_status_.front()));
            pending_database_status_.pop();
        }
    }

    if (to_send.empty()) {
        co_return;
    }

    BOOST_LOG_SEV(lg(), debug)
        << "Sending " << to_send.size() << " pending database status notification(s)";

    for (const auto& status : to_send) {
        try {
            messaging::database_status_message msg{
                .available = status.available,
                .error_message = status.error_message,
                .timestamp = status.timestamp
            };

            auto payload = msg.serialize();
            messaging::frame frame{
                messaging::message_type::database_status_notification,
                ++sequence_number_,
                std::move(payload)
            };

            co_await write_frame_serialized(frame);

            BOOST_LOG_SEV(lg(), info)
                << "Sent database status notification (available="
                << status.available << ") to " << conn_->remote_address();

        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to send database status notification: " << e.what();
        }
    }
}

void server_session::register_with_subscription_manager() {
    if (!subscription_mgr_) {
        BOOST_LOG_SEV(lg(), debug)
            << "No subscription manager configured - skipping registration";
        return;
    }

    auto remote_addr = conn_->remote_address();
    BOOST_LOG_SEV(lg(), info)
        << "Registering session '" << remote_addr
        << "' with subscription manager";

    // Create a weak reference to this session to avoid preventing destruction
    // Note: This is safe because we unregister before the session is destroyed
    subscription_mgr_->register_session(remote_addr,
        [this](const std::string& event_type,
               std::chrono::system_clock::time_point timestamp,
               const std::vector<std::string>& entity_ids) {
            return queue_notification(event_type, timestamp, entity_ids);
        });
}

void server_session::unregister_from_subscription_manager() {
    if (!subscription_mgr_) {
        return;
    }

    auto remote_addr = conn_->remote_address();
    BOOST_LOG_SEV(lg(), info)
        << "Unregistering session '" << remote_addr
        << "' from subscription manager";

    subscription_mgr_->unregister_session(remote_addr);
}

boost::asio::awaitable<void> server_session::write_frame_serialized(
    const messaging::frame& frame) {
    // Wait for any in-progress write to complete (simple spinlock with yield)
    auto executor = co_await boost::asio::this_coro::executor;
    while (write_in_progress_.exchange(true, std::memory_order_acquire)) {
        // Another write is in progress, yield and retry
        boost::asio::steady_timer timer(executor);
        timer.expires_after(std::chrono::microseconds(100));
        co_await timer.async_wait(boost::asio::use_awaitable);
    }

    try {
        co_await conn_->write_frame(frame);
        write_in_progress_.store(false, std::memory_order_release);
    } catch (...) {
        write_in_progress_.store(false, std::memory_order_release);
        throw;
    }
}

}
