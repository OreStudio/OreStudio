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
            // Emit signal on main thread via meta-object system?
            // No, signals are thread-safe but slot invocation depends on connection type.
            // We'll rely on QObject thread affinity.
            QMetaObject::invokeMethod(this, "disconnected", Qt::QueuedConnection);
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

        // Success - swap in new client
        client_ = new_client;
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
        client_->disconnect();
        client_.reset();
        emit disconnected();
    }
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

}
