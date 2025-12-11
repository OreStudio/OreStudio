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
#include "ores.comms/net/session.hpp"

#include <boost/asio/redirect_error.hpp>
#include <boost/asio/experimental/awaitable_operators.hpp>
#include "ores.comms/messaging/handshake_protocol.hpp"
#include "ores.comms/messaging/subscription_protocol.hpp"
#include "ores.comms/service/handshake_service.hpp"
#include "ores.comms/service/heartbeat_service.hpp"
#include "ores.comms/service/subscription_manager.hpp"

using namespace boost::asio::experimental::awaitable_operators;

namespace ores::comms::net {

using namespace ores::utility::log;

session::session(std::unique_ptr<connection> conn, std::string server_id,
    std::shared_ptr<messaging::message_dispatcher> dispatcher,
    boost::asio::any_io_executor io_executor,
    std::shared_ptr<service::subscription_manager> subscription_mgr)
    : conn_(std::move(conn)),
      server_id_(std::move(server_id)),
      dispatcher_(std::move(dispatcher)),
      subscription_mgr_(std::move(subscription_mgr)),
      sequence_number_(0),
      handshake_complete_(false),
      write_strand_(boost::asio::make_strand(io_executor)),
      notification_signal_(io_executor) {
    // Set timer to "never expire" initially - it will be cancelled to signal
    notification_signal_.expires_at(std::chrono::steady_clock::time_point::max());
}

void session::stop() {
    BOOST_LOG_SEV(lg(), info) << "Stopping session for " << conn_->remote_address();
    active_ = false;
    if (conn_) {
        conn_->close();
    }
}

bool session::queue_notification(const std::string& event_type,
    std::chrono::system_clock::time_point timestamp) {
    if (!active_) {
        BOOST_LOG_SEV(lg(), debug)
            << "Cannot queue notification - session not active";
        return false;
    }

    {
        std::lock_guard lock(notification_mutex_);
        pending_notifications_.push({event_type, timestamp});
    }

    // Signal the notification writer by cancelling the timer
    // This will wake up the waiting coroutine immediately
    notification_signal_.cancel();

    BOOST_LOG_SEV(lg(), debug)
        << "Queued notification for event type '" << event_type << "'";
    return true;
}

boost::asio::awaitable<void> session::run() {
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

boost::asio::awaitable<bool> session::perform_handshake() {
    bool success = co_await service::handshake_service::perform_server_handshake(
        *conn_, ++sequence_number_, server_id_);

    if (success) {
        handshake_complete_ = true;
    }

    co_return success;
}

boost::asio::awaitable<void> session::process_messages() {
    BOOST_LOG_SEV(lg(), debug) << "Starting message processing loop";

    try {
        while (active_) {
            // Read next message frame
            auto frame_result = co_await conn_->read_frame(false);
            if (!frame_result) {
                auto err = frame_result.error();
                if (err == messaging::error_code::network_error) {
                    BOOST_LOG_SEV(lg(), info) << "Client disconnected";
                } else {
                    BOOST_LOG_SEV(lg(), error) << "Failed to read frame: "
                                              << static_cast<int>(err);
                }
                co_return;
            }

            const auto& request_frame = *frame_result;
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
                    ++sequence_number_, remote_addr);

                if (!response_result) {
                    auto err = response_result.error();
                    BOOST_LOG_SEV(lg(), error) << "Message dispatch failed: "
                                              << static_cast<int>(err);

                    // Create error response with appropriate message
                    std::string error_msg;
                    switch (err) {
                        case messaging::error_code::invalid_message_type:
                            error_msg = "Invalid or unsupported message type";
                            break;
                        case messaging::error_code::handler_error:
                            error_msg = "Request handler encountered an error";
                            break;
                        case messaging::error_code::database_error:
                            error_msg = "Database operation failed";
                            break;
                        case messaging::error_code::authentication_failed:
                            error_msg = "Authentication failed";
                            break;
                        case messaging::error_code::authorization_failed:
                            error_msg = "Authorization failed";
                            break;
                        case messaging::error_code::invalid_request:
                            error_msg = "Invalid request parameters";
                            break;
                        case messaging::error_code::bootstrap_mode_only:
                            error_msg = "System is in bootstrap mode. Only initial admin account creation is allowed. Please create the initial admin account from localhost.";
                            break;
                        case messaging::error_code::bootstrap_mode_forbidden:
                            error_msg = "Operation not allowed - system is not in bootstrap mode";
                            break;
                        case messaging::error_code::weak_password:
                            error_msg = "Password does not meet security requirements";
                            break;
                        case messaging::error_code::not_localhost:
                            error_msg = "Bootstrap operations are only allowed from localhost";
                            break;
                        default:
                            error_msg = "An error occurred processing your request";
                            break;
                    }

                    response_frame = messaging::create_error_response_frame(
                        sequence_number_, err, error_msg);
                    BOOST_LOG_SEV(lg(), debug) << "Sending error response: " << error_msg;
                } else {
                    response_frame = *response_result;
                }
            } catch (const std::exception& e) {
                // Handler threw an exception - send detailed error to client
                BOOST_LOG_SEV(lg(), error) << "Exception in message handler: " << e.what();
                response_frame = messaging::create_error_response_frame(
                    sequence_number_, messaging::error_code::database_error, e.what());
                BOOST_LOG_SEV(lg(), debug) << "Sending error response with exception details";
            }

            // Send response back to client
            co_await conn_->write_frame(response_frame);
            BOOST_LOG_SEV(lg(), debug) << "Sent response for message type "
                                      << request_frame.header().type;

            // Close connection after logout
            if (request_frame.header().type == messaging::message_type::logout_request) {
                BOOST_LOG_SEV(lg(), info) << "Logout completed, closing connection";
                co_return;
            }
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Exception in message processing: " << e.what();
    }
}

boost::asio::awaitable<void> session::run_notification_writer() {
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
            }
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Exception in notification writer: " << e.what();
    }

    BOOST_LOG_SEV(lg(), debug) << "Notification writer coroutine ended";
}

boost::asio::awaitable<void> session::send_pending_notifications() {
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
                .timestamp = notification.timestamp
            };

            auto payload = msg.serialize();
            messaging::frame frame{
                messaging::message_type::notification,
                ++sequence_number_,
                std::move(payload)
            };

            co_await conn_->write_frame(frame);

            BOOST_LOG_SEV(lg(), info)
                << "Sent notification for event type '" << notification.event_type
                << "' to " << conn_->remote_address();

        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to send notification for event type '"
                << notification.event_type << "': " << e.what();
        }
    }
}

void session::register_with_subscription_manager() {
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
               std::chrono::system_clock::time_point timestamp) {
            return queue_notification(event_type, timestamp);
        });
}

void session::unregister_from_subscription_manager() {
    if (!subscription_mgr_) {
        return;
    }

    auto remote_addr = conn_->remote_address();
    BOOST_LOG_SEV(lg(), info)
        << "Unregistering session '" << remote_addr
        << "' from subscription manager";

    subscription_mgr_->unregister_session(remote_addr);
}

}
