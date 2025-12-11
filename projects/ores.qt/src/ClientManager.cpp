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
#include <boost/asio/use_future.hpp>
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/handshake_protocol.hpp"
#include "ores.accounts/messaging/protocol.hpp"

namespace ores::qt {

using namespace ores::utility::log;

ClientManager::ClientManager(QObject* parent)
    : QObject(parent) {
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
            .verify_certificate = false
        };

        // Create new client using persistent IO executor
        auto new_client = std::make_shared<comms::net::client>(
            config, io_context_->get_executor());

        // Synchronous connect (blocking but called from async thread usually)
        new_client->connect_sync();

        // Set up disconnect callback
        new_client->set_disconnect_callback([this]() {
            BOOST_LOG_SEV(lg(), warn) << "Client detected disconnect";
            // Emit signal on main thread via meta-object system
            QMetaObject::invokeMethod(this, "disconnected", Qt::QueuedConnection);
        });

        // Set up reconnecting callback
        new_client->set_reconnecting_callback([this]() {
            BOOST_LOG_SEV(lg(), info) << "Client attempting to reconnect";
            // Emit signal on main thread via meta-object system
            QMetaObject::invokeMethod(this, "reconnecting", Qt::QueuedConnection);
        });

        // Set up reconnected callback
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
            return {false, QString("Network error during login request")};
        }

        // Check for error response
        if (response_result->header().type == comms::messaging::message_type::error_response) {
            auto error_resp = comms::messaging::error_response::deserialize(
                response_result->payload());
            if (error_resp) {
                return {false, QString::fromStdString(error_resp->message)};
            }
            return {false, QString("Unknown server error")};
        }

        auto response = accounts::messaging::login_response::deserialize(
            response_result->payload());

        if (!response || !response->success) {
            return {false, QString::fromStdString(
                response ? response->error_message : "Invalid login response")};
        }

        // Success - swap in new client and store account_id
        client_ = new_client;
        logged_in_account_id_ = response->account_id;
        BOOST_LOG_SEV(lg(), info) << "Login successful, account_id stored";

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
                auto qTimestamp = QDateTime::fromMSecsSinceEpoch(msecs, QTimeZone::UTC);

                QMetaObject::invokeMethod(this, [this, qEventType, qTimestamp]() {
                    emit notificationReceived(qEventType, qTimestamp);
                }, Qt::QueuedConnection);
            });

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

        accounts::messaging::logout_request request{
            .account_id = *logged_in_account_id_
        };

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

        auto response = accounts::messaging::logout_response::deserialize(
            response_result->payload());

        if (response && response->success) {
            BOOST_LOG_SEV(lg(), info) << "Logout successful";
            logged_in_account_id_ = std::nullopt;
            return true;
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Logout failed: "
                << (response ? response->message : "Invalid response");
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Logout exception: " << e.what();
    }

    logged_in_account_id_ = std::nullopt;
    return false;
}

bool ClientManager::isConnected() const {
    return client_ && client_->is_connected();
}

std::expected<comms::messaging::frame, comms::messaging::error_code>
ClientManager::sendRequest(comms::messaging::frame request) {
    if (!isConnected()) {
        return std::unexpected(comms::messaging::error_code::network_error);
    }
    return client_->send_request_sync(std::move(request));
}

bool ClientManager::subscribeToEvent(const std::string& eventType) {
    if (!event_adapter_) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot subscribe: no event adapter";
        return false;
    }

    BOOST_LOG_SEV(lg(), info) << "Subscribing to event: " << eventType;

    try {
        // Run subscribe coroutine synchronously via io_context
        auto task = [this, eventType]() -> boost::asio::awaitable<bool> {
            co_return co_await event_adapter_->subscribe(eventType);
        };

        auto future = boost::asio::co_spawn(
            io_context_->get_executor(), task(), boost::asio::use_future);

        return future.get();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Subscribe failed: " << e.what();
        return false;
    }
}

bool ClientManager::unsubscribeFromEvent(const std::string& eventType) {
    if (!event_adapter_) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot unsubscribe: no event adapter";
        return false;
    }

    BOOST_LOG_SEV(lg(), info) << "Unsubscribing from event: " << eventType;

    try {
        // Run unsubscribe coroutine synchronously via io_context
        auto task = [this, eventType]() -> boost::asio::awaitable<bool> {
            co_return co_await event_adapter_->unsubscribe(eventType);
        };

        auto future = boost::asio::co_spawn(
            io_context_->get_executor(), task(), boost::asio::use_future);

        return future.get();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Unsubscribe failed: " << e.what();
        return false;
    }
}

}
