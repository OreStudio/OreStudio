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
#include "ores.qt/ClientManager.hpp"

#include <QtConcurrent>
#include <QFuture>
#include <QTimeZone>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/use_future.hpp>
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/handshake_protocol.hpp"
#include "ores.comms/domain/events/connection_events.hpp"
#include "ores.accounts/messaging/protocol.hpp"

namespace ores::qt {

using namespace ores::utility::log;

ClientManager::ClientManager(std::shared_ptr<eventing::service::event_bus> event_bus,
                             QObject* parent)
    : QObject(parent), event_bus_(std::move(event_bus)) {
    BOOST_LOG_SEV(lg(), debug) << "ClientManager created";
    setupIO();
}

ClientManager::~ClientManager() {
    BOOST_LOG_SEV(lg(), debug) << "ClientManager destroyed";
    disconnect();
    cleanupIO();
}

void ClientManager::setupIO() {
    if (io_context_) return;

    BOOST_LOG_SEV(lg(), debug) << "Setting up persistent IO context";
    io_context_ = std::make_unique<boost::asio::io_context>();
    work_guard_ = std::make_unique<boost::asio::executor_work_guard<
        boost::asio::io_context::executor_type>>(
        boost::asio::make_work_guard(*io_context_)
    );

    io_thread_ = std::make_unique<std::thread>([this]() {
        BOOST_LOG_SEV(lg(), debug) << "IO thread started";
        io_context_->run();
        BOOST_LOG_SEV(lg(), debug) << "IO thread finished";
    });
}

void ClientManager::cleanupIO() {
    work_guard_.reset();
    if (io_context_) {
        io_context_->stop();
    }
    if (io_thread_ && io_thread_->joinable()) {
        io_thread_->join();
    }
    io_thread_.reset();
    io_context_.reset();
}

std::pair<bool, QString> ClientManager::connectAndLogin(
    const std::string& host,
    std::uint16_t port,
    const std::string& username,
    const std::string& password) {

    BOOST_LOG_SEV(lg(), info) << "Connecting to " << host << ":" << port;

    // If already connected, disconnect first
    if (client_ && client_->is_connected()) {
        disconnect();
    }

    try {
        comms::net::client_options config{
            .host = host,
            .port = port,
            .client_identifier = "ores-qt-client",
            .verify_certificate = false,
            .supported_compression = supported_compression_
        };

        // Create new client using persistent IO executor.
        // Note: We don't pass event_bus here because we don't want connected_event
        // published until login succeeds. We'll publish events manually after login.
        auto new_client = std::make_shared<comms::net::client>(
            config, io_context_->get_executor(), nullptr);

        // Synchronous connect (blocking but called from async thread usually)
        new_client->connect_sync();

        // Set up disconnect callback (events are now published by the client directly)
        new_client->set_disconnect_callback([this]() {
            BOOST_LOG_SEV(lg(), warn) << "Client detected disconnect";
            // Emit signal on main thread via meta-object system
            QMetaObject::invokeMethod(this, "disconnected", Qt::QueuedConnection);
        });

        // Set up reconnecting callback (events are now published by the client directly)
        new_client->set_reconnecting_callback([this]() {
            BOOST_LOG_SEV(lg(), info) << "Client attempting to reconnect";
            // Emit signal on main thread via meta-object system
            QMetaObject::invokeMethod(this, "reconnecting", Qt::QueuedConnection);
        });

        // Set up reconnected callback (events are now published by the client directly)
        new_client->set_reconnected_callback([this]() {
            BOOST_LOG_SEV(lg(), info) << "Client reconnected successfully";
            // Emit signal on main thread via meta-object system
            QMetaObject::invokeMethod(this, "reconnected", Qt::QueuedConnection);
        });

        // Perform Login
        accounts::messaging::login_request request{
            .username = username,
            .password = password
        };

        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::login_request,
            0,
            std::move(payload)
        );

        auto response_result = new_client->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "LOGIN FAILURE: Network error during login request"
                                       << ", error_code: "
                                       << static_cast<int>(response_result.error());
            return {false, QString("Network error during login request")};
        }

        // Log frame attributes for debugging
        const auto& header = response_result->header();
        BOOST_LOG_SEV(lg(), debug) << "Login response frame: type="
                                   << static_cast<int>(header.type)
                                   << ", compression=" << header.compression
                                   << ", payload_size=" << response_result->payload().size()
                                   << ", correlation_id=" << response_result->correlation_id();

        // Decompress payload
        auto response_payload_result = response_result->decompressed_payload();
        if (!response_payload_result) {
            BOOST_LOG_SEV(lg(), error) << "LOGIN FAILURE: Failed to decompress response"
                                       << ", compression=" << header.compression
                                       << ", error=" << response_payload_result.error();
            return {false, QString("Failed to decompress server response")};
        }
        const auto& response_payload = *response_payload_result;

        // Check for error response
        if (header.type == comms::messaging::message_type::error_response) {
            auto error_resp = comms::messaging::error_response::deserialize(response_payload);
            if (error_resp) {
                BOOST_LOG_SEV(lg(), error) << "LOGIN FAILURE: Server returned error response"
                                           << ", error_code: "
                                           << static_cast<int>(error_resp->code)
                                           << ", message: " << error_resp->message;
                return {false, QString::fromStdString(error_resp->message)};
            }
            BOOST_LOG_SEV(lg(), error) << "LOGIN FAILURE: Server returned malformed error response";
            return {false, QString("Unknown server error")};
        }

        auto response = accounts::messaging::login_response::deserialize(response_payload);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "LOGIN FAILURE: Failed to deserialize login_response"
                                       << ", decompressed_payload_size: " << response_payload.size();
            return {false, QString("Invalid login response from server")};
        }

        if (!response->success) {
            BOOST_LOG_SEV(lg(), warn) << "LOGIN FAILURE: Server rejected login for user '"
                                      << username << "', reason: " << response->error_message;
            return {false, QString::fromStdString(response->error_message)};
        }

        // Success - swap in new client and store account_id and admin status
        client_ = new_client;
        logged_in_account_id_ = response->account_id;
        is_admin_ = response->is_admin;
        connected_host_ = host;
        connected_port_ = port;
        BOOST_LOG_SEV(lg(), info) << "LOGIN SUCCESS: User '" << response->username
                                  << "' authenticated to " << host << ":" << port
                                  << ", is_admin: " << response->is_admin;

        // Create event adapter for subscriptions
        event_adapter_ = std::make_unique<comms::service::remote_event_adapter>(client_);
        event_adapter_->set_notification_callback(
            [this](const std::string& event_type,
                   std::chrono::system_clock::time_point timestamp) {
                BOOST_LOG_SEV(lg(), debug) << "Received notification for " << event_type;
                // Convert to Qt types and emit on main thread
                auto qEventType = QString::fromStdString(event_type);
                auto msecs = std::chrono::duration_cast<std::chrono::milliseconds>(
                    timestamp.time_since_epoch()).count();
                auto qTimestamp = QDateTime::fromMSecsSinceEpoch(msecs, QTimeZone::utc());

                QMetaObject::invokeMethod(this, [this, qEventType, qTimestamp]() {
                    emit notificationReceived(qEventType, qTimestamp);
                }, Qt::QueuedConnection);
            });

        // Publish connected event to event bus now that login succeeded
        if (event_bus_) {
            event_bus_->publish(comms::domain::events::connected_event{
                .timestamp = std::chrono::system_clock::now(),
                .host = host,
                .port = port
            });
        }
        emit connected();
        return {true, QString()};

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Connection failed: " << e.what();
        return {false, QString::fromStdString(e.what())};
    }
}

void ClientManager::disconnect() {
    if (client_) {
        BOOST_LOG_SEV(lg(), info) << "Disconnecting client";

        // Send logout request before disconnecting
        logout();

        // Clean up event adapter before disconnecting
        event_adapter_.reset();

        // The server closes the connection after logout, but we call disconnect
        // to ensure proper cleanup on the client side
        client_->disconnect();
        client_.reset();

        // Disconnected event is now published by the client directly
        emit disconnected();
    }
}

bool ClientManager::logout() {
    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), debug) << "Not connected, nothing to logout";
        return false;
    }

    if (!logged_in_account_id_) {
        BOOST_LOG_SEV(lg(), debug) << "No logged-in account, skipping logout";
        return false;
    }

    try {
        BOOST_LOG_SEV(lg(), debug) << "Sending logout request";

        // logout_request is empty - server determines account from session context
        accounts::messaging::logout_request request{};

        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::logout_request,
            0,
            std::move(payload)
        );

        auto response_result = client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), warn) << "Logout request failed (network error)";
            logged_in_account_id_ = std::nullopt;
            return false;
        }

        // Log frame attributes for debugging
        const auto& header = response_result->header();
        BOOST_LOG_SEV(lg(), debug) << "Logout response frame: type="
                                   << static_cast<int>(header.type)
                                   << ", compression=" << header.compression
                                   << ", payload_size=" << response_result->payload().size();

        // Decompress payload
        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), warn) << "Logout failed: decompression error";
            logged_in_account_id_ = std::nullopt;
            return false;
        }

        auto response = accounts::messaging::logout_response::deserialize(*payload_result);

        if (response && response->success) {
            BOOST_LOG_SEV(lg(), info) << "Logout successful";
            logged_in_account_id_ = std::nullopt;
            is_admin_ = false;
            return true;
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Logout failed: "
                << (response ? response->message : "Invalid response");
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Logout exception: " << e.what();
    }

    logged_in_account_id_ = std::nullopt;
    is_admin_ = false;
    return false;
}

bool ClientManager::isConnected() const {
    return client_ && client_->is_connected();
}

bool ClientManager::isAdmin() const {
    return is_admin_;
}

std::expected<comms::messaging::frame, comms::messaging::error_code>
ClientManager::sendRequest(comms::messaging::frame request) {
    if (!isConnected()) {
        return std::unexpected(comms::messaging::error_code::network_error);
    }
    return client_->send_request_sync(std::move(request));
}

void ClientManager::subscribeToEvent(const std::string& eventType) {
    if (!event_adapter_) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot subscribe: no event adapter";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Subscribing to event: " << eventType;

    // Run subscribe coroutine asynchronously to avoid blocking the GUI thread
    auto task = [this, eventType]() -> boost::asio::awaitable<void> {
        try {
            if (!co_await event_adapter_->subscribe(eventType)) {
                BOOST_LOG_SEV(lg(), error) << "Subscription failed for " << eventType;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Subscribe failed with exception: " << e.what();
        }
    };

    boost::asio::co_spawn(
        io_context_->get_executor(), std::move(task), boost::asio::detached);
}

void ClientManager::unsubscribeFromEvent(const std::string& eventType) {
    if (!event_adapter_) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot unsubscribe: no event adapter";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Unsubscribing from event: " << eventType;

    // Run unsubscribe coroutine asynchronously to avoid blocking the GUI thread
    auto task = [this, eventType]() -> boost::asio::awaitable<void> {
        try {
            if (!co_await event_adapter_->unsubscribe(eventType)) {
                BOOST_LOG_SEV(lg(), error) << "Unsubscription failed for " << eventType;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Unsubscribe failed with exception: " << e.what();
        }
    };

    boost::asio::co_spawn(
        io_context_->get_executor(), std::move(task), boost::asio::detached);
}

}
