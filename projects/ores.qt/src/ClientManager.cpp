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
#include <QThreadPool>
#include <QTimeZone>
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/messaging/message_type.hpp"
#include "ores.comms/messaging/handshake_protocol.hpp"
#include "ores.comms/eventing/connection_events.hpp"
#include "ores.iam/messaging/protocol.hpp"
#include "ores.iam/messaging/signup_protocol.hpp"
#include "ores.iam/messaging/session_protocol.hpp"
#include "ores.iam/messaging/bootstrap_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

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

LoginResult ClientManager::connect(const std::string& host, std::uint16_t port) {
    BOOST_LOG_SEV(lg(), info) << "Connecting to " << host << ":" << port;

    // Reset user disconnect flag for fresh connection
    user_disconnecting_.store(false, std::memory_order_release);

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

        // Set up reconnecting callback - emits signal on Qt thread
        // The user_disconnecting_ flag prevents spurious signals during user-initiated disconnect
        new_client->set_reconnecting_callback([this]() {
            BOOST_LOG_SEV(lg(), info) << "Client attempting to reconnect";
            QMetaObject::invokeMethod(this, [this]() {
                if (user_disconnecting_.load(std::memory_order_acquire)) {
                    BOOST_LOG_SEV(lg(), debug)
                        << "Suppressing reconnecting signal - user disconnect in progress";
                    return;
                }
                emit reconnecting();
            }, Qt::QueuedConnection);
        });

        // Set up reconnected callback - re-authenticate and emit signal on Qt thread
        new_client->set_reconnected_callback([this]() {
            BOOST_LOG_SEV(lg(), info) << "Client reconnected, re-authenticating...";

            if (user_disconnecting_.load(std::memory_order_acquire)) {
                BOOST_LOG_SEV(lg(), debug)
                    << "Suppressing re-authentication - user disconnect in progress";
                return;
            }

            // Re-authenticate using stored credentials
            if (stored_username_.empty()) {
                BOOST_LOG_SEV(lg(), warn) << "No stored credentials for re-authentication";
                QMetaObject::invokeMethod(this, [this]() {
                    emit connectionError(tr("Reconnected but no credentials available for re-authentication"));
                }, Qt::QueuedConnection);
                return;
            }

            // Perform login request
            // stored_username_ acts as principal (can be "user" or "user@hostname")
            iam::messaging::login_request request{
                .principal = stored_username_,
                .password = stored_password_
            };

            auto payload = request.serialize();
            comms::messaging::frame request_frame(
                comms::messaging::message_type::login_request,
                0,
                std::move(payload)
            );

            auto response_result = client_->send_request_sync(std::move(request_frame));

            if (!response_result) {
                BOOST_LOG_SEV(lg(), error) << "Re-authentication failed: network error";
                QMetaObject::invokeMethod(this, [this]() {
                    emit connectionError(tr("Re-authentication failed after reconnection"));
                }, Qt::QueuedConnection);
                return;
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                BOOST_LOG_SEV(lg(), error) << "Re-authentication failed: decompression error";
                QMetaObject::invokeMethod(this, [this]() {
                    emit connectionError(tr("Re-authentication failed after reconnection"));
                }, Qt::QueuedConnection);
                return;
            }

            auto response = iam::messaging::login_response::deserialize(*payload_result);
            if (!response || !response->success) {
                BOOST_LOG_SEV(lg(), error) << "Re-authentication failed: server rejected login";
                QMetaObject::invokeMethod(this, [this]() {
                    emit connectionError(tr("Re-authentication failed - please log in again"));
                }, Qt::QueuedConnection);
                return;
            }

            // Update session info
            session_.set_session_info(comms::net::client_session_info{
                .account_id = response->account_id,
                .tenant_id = response->tenant_id,
                .username = response->username,
                .email = response->email
            });

            BOOST_LOG_SEV(lg(), info) << "Re-authentication successful for user '"
                                      << response->username << "'";

            QMetaObject::invokeMethod(this, [this]() {
                if (user_disconnecting_.load(std::memory_order_acquire)) {
                    BOOST_LOG_SEV(lg(), debug)
                        << "Suppressing reconnected signal - user disconnect in progress";
                    return;
                }
                emit reconnected();
            }, Qt::QueuedConnection);
        });

        // Check bootstrap status before attempting login
        // The bootstrap_status_request does not require authentication
        BOOST_LOG_SEV(lg(), debug) << "Checking bootstrap status...";
        iam::messaging::bootstrap_status_request bootstrap_request;
        auto bootstrap_payload = bootstrap_request.serialize();
        comms::messaging::frame bootstrap_frame(
            comms::messaging::message_type::bootstrap_status_request,
            0,
            std::move(bootstrap_payload)
        );

        auto bootstrap_response_result = new_client->send_request_sync(std::move(bootstrap_frame));
        if (bootstrap_response_result) {
            auto bootstrap_payload_result = bootstrap_response_result->decompressed_payload();
            if (bootstrap_payload_result) {
                auto bootstrap_response = iam::messaging::bootstrap_status_response::deserialize(
                    *bootstrap_payload_result);
                if (bootstrap_response && bootstrap_response->is_in_bootstrap_mode) {
                    BOOST_LOG_SEV(lg(), info)
                        << "System is in bootstrap mode - skipping login";

                    // Store the client and connection info even though we're not logging in
                    client_ = new_client;
                    auto attach_result = session_.attach_client(client_);
                    if (!attach_result) {
                        BOOST_LOG_SEV(lg(), error) << "Failed to attach client to session";
                        client_->disconnect();
                        client_.reset();
                        return {.success = false, .error_message = QString("Failed to initialize session")};
                    }
                    connected_host_ = host;
                    connected_port_ = port;

                    // Emit connected signal
                    QMetaObject::invokeMethod(this, "connected", Qt::QueuedConnection);

                    return {
                        .success = false,
                        .error_message = QString::fromStdString(bootstrap_response->message),
                        .password_reset_required = false,
                        .bootstrap_mode = true
                    };
                }
            }
        }
        BOOST_LOG_SEV(lg(), debug) << "System is not in bootstrap mode, ready for login";

        // Store the client and connection info - ready for login()
        client_ = new_client;
        auto attach_result = session_.attach_client(client_);
        if (!attach_result) {
            BOOST_LOG_SEV(lg(), error) << "Failed to attach client to session";
            client_->disconnect();
            client_.reset();
            return {.success = false, .error_message = QString("Failed to initialize session")};
        }
        connected_host_ = host;
        connected_port_ = port;

        // Emit connected signal
        QMetaObject::invokeMethod(this, "connected", Qt::QueuedConnection);

        return {.success = true, .error_message = QString()};

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Connection failed: " << e.what();
        return {.success = false, .error_message = QString::fromStdString(e.what())};
    }
}

LoginResult ClientManager::login(const std::string& username, const std::string& password) {
    BOOST_LOG_SEV(lg(), info) << "Logging in as " << username;

    if (!client_ || !client_->is_connected()) {
        BOOST_LOG_SEV(lg(), error) << "LOGIN FAILURE: Not connected";
        return {.success = false, .error_message = QString("Not connected to server")};
    }

    try {
        // Perform Login
        // username acts as principal (can be "user" or "user@hostname")
        iam::messaging::login_request request{
            .principal = username,
            .password = password
        };

        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::login_request,
            0,
            std::move(payload)
        );

        auto response_result = client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "LOGIN FAILURE: Network error during login request"
                                       << ", error_code: "
                                       << static_cast<int>(response_result.error());
            return {.success = false, .error_message = QString("Network error during login request")};
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
            return {.success = false, .error_message = QString("Failed to decompress server response")};
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
                return {.success = false, .error_message = QString::fromStdString(error_resp->message)};
            }
            BOOST_LOG_SEV(lg(), error) << "LOGIN FAILURE: Server returned malformed error response";
            return {.success = false, .error_message = QString("Unknown server error")};
        }

        auto response = iam::messaging::login_response::deserialize(response_payload);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "LOGIN FAILURE: Failed to deserialize login_response"
                                       << ", decompressed_payload_size: " << response_payload.size();
            return {.success = false, .error_message = QString("Invalid login response from server")};
        }

        if (!response->success) {
            BOOST_LOG_SEV(lg(), warn) << "LOGIN FAILURE: Server rejected login for user '"
                                      << username << "', reason: " << response->error_message;
            return {.success = false, .error_message = QString::fromStdString(response->error_message)};
        }

        const bool password_reset_required = response->password_reset_required;
        const bool tenant_bootstrap = response->tenant_bootstrap_mode;

        // Store credentials for re-authentication after reconnection
        stored_username_ = username;
        stored_password_ = password;

        // Set session info
        session_.set_session_info(comms::net::client_session_info{
            .account_id = response->account_id,
            .tenant_id = response->tenant_id,
            .username = response->username,
            .email = response->email
        });

        // Set notification callback to emit Qt signals
        session_.set_notification_callback(
            [this](const std::string& event_type,
                   std::chrono::system_clock::time_point timestamp,
                   const std::vector<std::string>& entity_ids,
                   const std::string& tenant_id) {
                BOOST_LOG_SEV(lg(), debug) << "Received notification for " << event_type
                                           << " with " << entity_ids.size() << " entity IDs"
                                           << ", tenant: " << tenant_id;
                // Convert to Qt types and emit on main thread
                auto qEventType = QString::fromStdString(event_type);
                auto msecs = std::chrono::duration_cast<std::chrono::milliseconds>(
                    timestamp.time_since_epoch()).count();
                auto qTimestamp = QDateTime::fromMSecsSinceEpoch(msecs, QTimeZone::utc());
                QStringList qEntityIds;
                qEntityIds.reserve(static_cast<int>(entity_ids.size()));
                for (const auto& id : entity_ids) {
                    qEntityIds.append(QString::fromStdString(id));
                }
                auto qTenantId = QString::fromStdString(tenant_id);

                QMetaObject::invokeMethod(this,
                    [this, qEventType, qTimestamp, qEntityIds, qTenantId]() {
                    emit notificationReceived(qEventType, qTimestamp, qEntityIds, qTenantId);
                }, Qt::QueuedConnection);
            });

        BOOST_LOG_SEV(lg(), info) << "LOGIN SUCCESS: User '" << response->username
                                  << "' authenticated to " << connected_host_ << ":" << connected_port_
                                  << ", password_reset_required: " << password_reset_required;

        // Enable recording if it was requested before connection
        if (recording_enabled_ && !recording_directory_.empty()) {
            BOOST_LOG_SEV(lg(), info) << "Enabling pre-configured recording to: "
                                      << recording_directory_;
            auto result = client_->enable_recording(recording_directory_);
            if (result) {
                BOOST_LOG_SEV(lg(), info) << "Recording started: " << result->string();
                QMetaObject::invokeMethod(this, [this, path = result->string()]() {
                    emit recordingStarted(QString::fromStdString(path));
                }, Qt::QueuedConnection);
            } else {
                BOOST_LOG_SEV(lg(), error) << "Failed to enable recording: "
                                           << static_cast<int>(result.error());
            }
        }

        // Enable telemetry streaming if it was requested before connection
        if (streaming_enabled_ && pending_streaming_options_) {
            BOOST_LOG_SEV(lg(), info) << "Enabling pre-configured telemetry streaming for: "
                                      << pending_streaming_options_->source_name;
            try {
                telemetry_streaming_ =
                    std::make_unique<comms::service::telemetry_streaming_service>(
                        client_, *pending_streaming_options_);
                telemetry_streaming_->start();
                BOOST_LOG_SEV(lg(), info) << "Telemetry streaming started";
                QMetaObject::invokeMethod(this, [this]() {
                    emit streamingStarted();
                }, Qt::QueuedConnection);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error) << "Failed to enable streaming: " << e.what();
            }
        }

        // Publish connected event to event bus now that login succeeded
        if (event_bus_) {
            event_bus_->publish(comms::eventing::connected_event{
                .timestamp = std::chrono::system_clock::now(),
                .host = connected_host_,
                .port = connected_port_
            });
        }
        emit loggedIn();
        return {.success = true, .error_message = QString(),
            .password_reset_required = password_reset_required,
            .tenant_bootstrap_mode = tenant_bootstrap};

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Login failed: " << e.what();
        return {.success = false, .error_message = QString::fromStdString(e.what())};
    }
}

LoginResult ClientManager::connectAndLogin(
    const std::string& host,
    std::uint16_t port,
    const std::string& username,
    const std::string& password) {

    // First, establish connection
    auto connect_result = connect(host, port);

    // If bootstrap mode, return early
    if (connect_result.bootstrap_mode) {
        return connect_result;
    }

    // If connection failed, return the error
    if (!connect_result.success) {
        return connect_result;
    }

    // Now perform login
    return login(username, password);
}

LoginResult ClientManager::testConnection(
    const std::string& host,
    std::uint16_t port,
    const std::string& username,
    const std::string& password) {

    BOOST_LOG_SEV(lg(), info) << "Testing connection to " << host << ":" << port;

    try {
        comms::net::client_options config{
            .host = host,
            .port = port,
            .client_identifier = "ores-qt-client",
            .verify_certificate = false,
            .supported_compression = supported_compression_
        };

        // Create temporary client for testing (not stored in client_)
        auto temp_client = std::make_shared<comms::net::client>(
            config, io_context_->get_executor(), nullptr);

        // Synchronous connect
        temp_client->connect_sync();

        // Perform Login
        // username acts as principal (can be "user" or "user@hostname")
        iam::messaging::login_request request{
            .principal = username,
            .password = password
        };

        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::login_request,
            0,
            std::move(payload)
        );

        auto response_result = temp_client->send_request_sync(std::move(request_frame));

        // Disconnect temporary client immediately
        temp_client->disconnect();

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "TEST CONNECTION FAILURE: Network error"
                                       << ", error_code: "
                                       << static_cast<int>(response_result.error());
            return {.success = false, .error_message = QString("Network error during login request")};
        }

        const auto& header = response_result->header();

        // Decompress payload
        auto response_payload_result = response_result->decompressed_payload();
        if (!response_payload_result) {
            BOOST_LOG_SEV(lg(), error) << "TEST CONNECTION FAILURE: Decompression error";
            return {.success = false, .error_message = QString("Failed to decompress server response")};
        }
        const auto& response_payload = *response_payload_result;

        // Check for error response
        if (header.type == comms::messaging::message_type::error_response) {
            auto error_resp = comms::messaging::error_response::deserialize(response_payload);
            if (error_resp) {
                BOOST_LOG_SEV(lg(), warn) << "TEST CONNECTION FAILURE: " << error_resp->message;
                return {.success = false, .error_message = QString::fromStdString(error_resp->message)};
            }
            return {.success = false, .error_message = QString("Unknown server error")};
        }

        auto response = iam::messaging::login_response::deserialize(response_payload);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "TEST CONNECTION FAILURE: Invalid response";
            return {.success = false, .error_message = QString("Invalid login response from server")};
        }

        if (!response->success) {
            BOOST_LOG_SEV(lg(), warn) << "TEST CONNECTION FAILURE: " << response->error_message;
            return {.success = false, .error_message = QString::fromStdString(response->error_message)};
        }

        BOOST_LOG_SEV(lg(), info) << "TEST CONNECTION SUCCESS for user '" << username << "'";
        return {.success = true, .error_message = QString()};

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Test connection failed: " << e.what();
        return {.success = false, .error_message = QString::fromStdString(e.what())};
    }
}

SignupResult ClientManager::signup(
    const std::string& host,
    std::uint16_t port,
    const std::string& username,
    const std::string& email,
    const std::string& password) {

    BOOST_LOG_SEV(lg(), info) << "Attempting signup to " << host << ":" << port
                              << " for username: " << username;

    try {
        comms::net::client_options config{
            .host = host,
            .port = port,
            .client_identifier = "ores-qt-client",
            .verify_certificate = false,
            .supported_compression = supported_compression_
        };

        // Create temporary client for signup (not stored in client_)
        auto temp_client = std::make_shared<comms::net::client>(
            config, io_context_->get_executor(), nullptr);

        // Synchronous connect
        temp_client->connect_sync();

        // Send signup request
        iam::messaging::signup_request request{
            .username = username,
            .email = email,
            .password = password
        };

        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::signup_request,
            0,
            std::move(payload)
        );

        auto response_result = temp_client->send_request_sync(std::move(request_frame));

        // Disconnect temporary client
        temp_client->disconnect();

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "SIGNUP FAILURE: Network error during signup request"
                                       << ", error_code: "
                                       << static_cast<int>(response_result.error());
            return {.success = false,
                    .error_message = QString("Network error during signup request"),
                    .username = QString()};
        }

        // Log frame attributes for debugging
        const auto& header = response_result->header();
        BOOST_LOG_SEV(lg(), debug) << "Signup response frame: type="
                                   << static_cast<int>(header.type)
                                   << ", compression=" << header.compression
                                   << ", payload_size=" << response_result->payload().size();

        // Decompress payload
        auto response_payload_result = response_result->decompressed_payload();
        if (!response_payload_result) {
            BOOST_LOG_SEV(lg(), error) << "SIGNUP FAILURE: Failed to decompress response";
            return {.success = false,
                    .error_message = QString("Failed to decompress server response"),
                    .username = QString()};
        }
        const auto& response_payload = *response_payload_result;

        // Check for error response
        if (header.type == comms::messaging::message_type::error_response) {
            auto error_resp = comms::messaging::error_response::deserialize(response_payload);
            if (error_resp) {
                BOOST_LOG_SEV(lg(), error) << "SIGNUP FAILURE: Server returned error response"
                                           << ", error_code: "
                                           << static_cast<int>(error_resp->code)
                                           << ", message: " << error_resp->message;
                return {.success = false,
                        .error_message = QString::fromStdString(error_resp->message),
                        .username = QString()};
            }
            BOOST_LOG_SEV(lg(), error) << "SIGNUP FAILURE: Server returned malformed error response";
            return {.success = false,
                    .error_message = QString("Unknown server error"),
                    .username = QString()};
        }

        auto response = iam::messaging::signup_response::deserialize(response_payload);

        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "SIGNUP FAILURE: Failed to deserialize signup_response";
            return {.success = false,
                    .error_message = QString("Invalid signup response from server"),
                    .username = QString()};
        }

        if (!response->success) {
            BOOST_LOG_SEV(lg(), warn) << "SIGNUP FAILURE: Server rejected signup for user '"
                                      << username << "', reason: " << response->error_message;
            return {.success = false,
                    .error_message = QString::fromStdString(response->error_message),
                    .username = QString::fromStdString(username)};
        }

        BOOST_LOG_SEV(lg(), info) << "SIGNUP SUCCESS: Account created for user '"
                                  << response->username << "'";
        return {.success = true,
                .error_message = QString(),
                .username = QString::fromStdString(response->username)};

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Signup failed: " << e.what();
        return {.success = false,
                .error_message = QString::fromStdString(e.what()),
                .username = QString()};
    }
}

void ClientManager::disconnect() {
    // Set flag FIRST to prevent any pending reconnecting signals from being emitted
    user_disconnecting_.store(true, std::memory_order_release);

    if (client_) {
        BOOST_LOG_SEV(lg(), info) << "Disconnecting client (user-initiated)";

        // Stop telemetry streaming before disconnecting
        if (telemetry_streaming_) {
            BOOST_LOG_SEV(lg(), info) << "Stopping telemetry streaming before disconnect. "
                                      << "Sent: " << telemetry_streaming_->total_sent()
                                      << ", dropped: " << telemetry_streaming_->total_dropped();
            telemetry_streaming_->stop();
            telemetry_streaming_.reset();
            emit streamingStopped();
        }

        // Send logout request before disconnecting
        logout();

        // Detach session (clears session state and event adapter)
        session_.detach_client();

        // Clear stored credentials
        stored_username_.clear();
        stored_password_.clear();

        // The server closes the connection after logout, but we call disconnect
        // to ensure proper cleanup on the client side
        client_->disconnect();

        // Wait for all client coroutines to complete before destroying
        // This prevents use-after-free crashes during rapid disconnect
        // Use a short timeout to avoid blocking the UI thread too long
        if (!client_->await_shutdown(std::chrono::milliseconds{500})) {
            BOOST_LOG_SEV(lg(), warn) << "Timeout waiting for client shutdown, "
                                      << "proceeding with cleanup anyway";
        }

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

    if (!session_.is_logged_in()) {
        BOOST_LOG_SEV(lg(), debug) << "No logged-in account, skipping logout";
        return false;
    }

    try {
        BOOST_LOG_SEV(lg(), debug) << "Sending logout request";

        // logout_request is empty - server determines account from session context
        iam::messaging::logout_request request{};

        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::logout_request,
            0,
            std::move(payload)
        );

        auto response_result = client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), warn) << "Logout request failed (network error)";
            session_.clear_session_info();
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
            session_.clear_session_info();
            return false;
        }

        auto response = iam::messaging::logout_response::deserialize(*payload_result);

        if (response && response->success) {
            BOOST_LOG_SEV(lg(), info) << "Logout successful";
            session_.clear_session_info();
            return true;
        } else {
            BOOST_LOG_SEV(lg(), warn) << "Logout failed: "
                << (response ? response->message : "Invalid response");
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Logout exception: " << e.what();
    }

    session_.clear_session_info();
    return false;
}

bool ClientManager::isConnected() const {
    return client_ && client_->is_connected();
}

bool ClientManager::isAdmin() const {
    // Deprecated: Permission checks are now performed server-side via RBAC
    return false;
}

std::expected<comms::messaging::frame, ores::utility::serialization::error_code>
ClientManager::sendRequest(comms::messaging::frame request) {
    if (!isConnected()) {
        return std::unexpected(ores::utility::serialization::error_code::network_error);
    }
    return client_->send_request_sync(std::move(request));
}

void ClientManager::subscribeToEvent(const std::string& eventType) {
    if (!session_.is_connected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot subscribe: not connected";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Subscribing to event: " << eventType;

    // Run subscription asynchronously to avoid blocking the GUI thread
    QThreadPool::globalInstance()->start([this, eventType]() {
        try {
            if (!session_.subscribe(eventType)) {
                BOOST_LOG_SEV(lg(), error) << "Subscription failed for " << eventType;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Subscribe failed with exception: " << e.what();
        }
    });
}

void ClientManager::unsubscribeFromEvent(const std::string& eventType) {
    if (!session_.is_connected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot unsubscribe: not connected";
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Unsubscribing from event: " << eventType;

    // Run unsubscription asynchronously to avoid blocking the GUI thread
    QThreadPool::globalInstance()->start([this, eventType]() {
        try {
            if (!session_.unsubscribe(eventType)) {
                BOOST_LOG_SEV(lg(), error) << "Unsubscription failed for " << eventType;
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Unsubscribe failed with exception: " << e.what();
        }
    });
}

std::optional<SessionListResult> ClientManager::listSessions(
    const boost::uuids::uuid& accountId,
    std::uint32_t limit,
    std::uint32_t offset) {

    if (!isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot list sessions: not connected";
        return std::nullopt;
    }

    try {
        iam::messaging::list_sessions_request request{
            .account_id = accountId,
            .limit = limit,
            .offset = offset
        };

        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::list_sessions_request,
            0,
            std::move(payload)
        );

        auto response_result = client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "List sessions failed: network error";
            return std::nullopt;
        }

        const auto& header = response_result->header();

        // Decompress payload
        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "List sessions failed: decompression error";
            return std::nullopt;
        }

        // Check for error response
        if (header.type == comms::messaging::message_type::error_response) {
            auto error_resp = comms::messaging::error_response::deserialize(*payload_result);
            BOOST_LOG_SEV(lg(), error) << "List sessions failed: "
                << (error_resp ? error_resp->message : "unknown error");
            return std::nullopt;
        }

        auto response = iam::messaging::list_sessions_response::deserialize(*payload_result);
        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "List sessions failed: invalid response";
            return std::nullopt;
        }

        BOOST_LOG_SEV(lg(), debug) << "Retrieved " << response->sessions.size()
                                   << " sessions (total: " << response->total_count << ")";

        return SessionListResult{
            .sessions = std::move(response->sessions),
            .total_count = response->total_count
        };

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "List sessions exception: " << e.what();
        return std::nullopt;
    }
}

std::optional<std::vector<iam::domain::session>> ClientManager::getActiveSessions() {
    if (!isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Cannot get active sessions: not connected";
        return std::nullopt;
    }

    try {
        iam::messaging::get_active_sessions_request request{};

        auto payload = request.serialize();
        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_active_sessions_request,
            0,
            std::move(payload)
        );

        auto response_result = client_->send_request_sync(std::move(request_frame));

        if (!response_result) {
            BOOST_LOG_SEV(lg(), error) << "Get active sessions failed: network error";
            return std::nullopt;
        }

        const auto& header = response_result->header();

        // Decompress payload
        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            BOOST_LOG_SEV(lg(), error) << "Get active sessions failed: decompression error";
            return std::nullopt;
        }

        // Check for error response
        if (header.type == comms::messaging::message_type::error_response) {
            auto error_resp = comms::messaging::error_response::deserialize(*payload_result);
            BOOST_LOG_SEV(lg(), error) << "Get active sessions failed: "
                << (error_resp ? error_resp->message : "unknown error");
            return std::nullopt;
        }

        auto response = iam::messaging::get_active_sessions_response::deserialize(*payload_result);
        if (!response) {
            BOOST_LOG_SEV(lg(), error) << "Get active sessions failed: invalid response";
            return std::nullopt;
        }

        BOOST_LOG_SEV(lg(), debug) << "Retrieved " << response->sessions.size()
                                   << " active sessions";

        return std::move(response->sessions);

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Get active sessions exception: " << e.what();
        return std::nullopt;
    }
}

// =============================================================================
// Session Recording
// =============================================================================

bool ClientManager::enableRecording(const std::filesystem::path& outputDirectory) {
    recording_directory_ = outputDirectory;
    recording_enabled_ = true;

    if (!client_) {
        // No client yet - recording will start when we connect
        BOOST_LOG_SEV(lg(), info) << "Recording enabled (will start on connect) to: "
                                  << outputDirectory;
        return true;
    }

    BOOST_LOG_SEV(lg(), info) << "Enabling session recording to: " << outputDirectory;

    auto result = client_->enable_recording(outputDirectory);
    if (!result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to enable recording: "
                                   << static_cast<int>(result.error());
        recording_enabled_ = false;
        return false;
    }

    BOOST_LOG_SEV(lg(), info) << "Recording started: " << result->string();
    emit recordingStarted(QString::fromStdString(result->string()));
    return true;
}

void ClientManager::disableRecording() {
    recording_enabled_ = false;

    if (!client_) {
        BOOST_LOG_SEV(lg(), debug) << "Recording disabled (was pending)";
        emit recordingStopped();
        return;
    }

    if (!client_->is_recording()) {
        BOOST_LOG_SEV(lg(), debug) << "Recording not active on client";
        emit recordingStopped();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Disabling session recording";
    client_->disable_recording();
    emit recordingStopped();
}

bool ClientManager::isRecording() const {
    return recording_enabled_;
}

std::filesystem::path ClientManager::recordingFilePath() const {
    if (!client_ || !client_->is_recording()) {
        return {};
    }
    // The client stores the recording file path in its session_recorder
    // We need to get it from there. For now, return empty since
    // client doesn't expose this directly. The signal contains the path.
    return {};
}

// =============================================================================
// Telemetry Streaming
// =============================================================================

void ClientManager::enableStreaming(
    const comms::service::telemetry_streaming_options& options) {

    streaming_enabled_ = true;
    pending_streaming_options_ = options;

    if (!client_) {
        // No client yet - streaming will start when we connect
        BOOST_LOG_SEV(lg(), info) << "Streaming enabled (will start on connect) for: "
                                  << options.source_name;
        return;
    }

    if (!client_->is_connected()) {
        BOOST_LOG_SEV(lg(), info) << "Streaming enabled (waiting for connection) for: "
                                  << options.source_name;
        return;
    }

    // Already connected - start streaming now
    BOOST_LOG_SEV(lg(), info) << "Enabling telemetry streaming for: "
                              << options.source_name;

    try {
        telemetry_streaming_ =
            std::make_unique<comms::service::telemetry_streaming_service>(
                client_, options);
        telemetry_streaming_->start();

        BOOST_LOG_SEV(lg(), info) << "Telemetry streaming started";
        emit streamingStarted();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to enable streaming: " << e.what();
        streaming_enabled_ = false;
        pending_streaming_options_.reset();
    }
}

void ClientManager::disableStreaming() {
    streaming_enabled_ = false;
    pending_streaming_options_.reset();

    if (!telemetry_streaming_) {
        BOOST_LOG_SEV(lg(), debug) << "Streaming disabled (was not active)";
        emit streamingStopped();
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Disabling telemetry streaming. "
                              << "Sent: " << telemetry_streaming_->total_sent()
                              << ", dropped: " << telemetry_streaming_->total_dropped();

    telemetry_streaming_->stop();
    telemetry_streaming_.reset();
    emit streamingStopped();
}

bool ClientManager::isStreaming() const {
    return telemetry_streaming_ && telemetry_streaming_->is_running();
}

std::size_t ClientManager::streamingPendingCount() const {
    return telemetry_streaming_ ? telemetry_streaming_->pending_count() : 0;
}

std::uint64_t ClientManager::streamingTotalSent() const {
    return telemetry_streaming_ ? telemetry_streaming_->total_sent() : 0;
}

std::uint64_t ClientManager::streamingTotalDropped() const {
    return telemetry_streaming_ ? telemetry_streaming_->total_dropped() : 0;
}

}
