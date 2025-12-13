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
#ifndef ORES_QT_CLIENT_MANAGER_HPP
#define ORES_QT_CLIENT_MANAGER_HPP

#include <chrono>
#include <memory>
#include <optional>
#include <string>
#include <thread>
#include <boost/asio/io_context.hpp>
#include <boost/asio/executor_work_guard.hpp>
#include <boost/uuid/uuid.hpp>
#include <QObject>
#include <QDateTime>
#include "ores.comms/net/client.hpp"
#include "ores.comms/service/remote_event_adapter.hpp"
#include "ores.eventing/service/event_bus.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Manages the lifecycle of the network client and IO context.
 *
 * Maintains a persistent IO context/thread while allowing the client connection
 * to be established and torn down repeatedly. Signals changes in connection
 * state to allow UI components to update accordingly without closing.
 */
class ClientManager : public QObject {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.client_manager";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit ClientManager(std::shared_ptr<eventing::service::event_bus> event_bus,
                           QObject* parent = nullptr);
    ~ClientManager() override;

    /**
     * @brief Connect to the server and perform login.
     *
     * @param host Server hostname
     * @param port Server port
     * @param username Login username
     * @param password Login password
     * @return std::pair<bool, QString> Success status and error message
     */
    std::pair<bool, QString> connectAndLogin(
        const std::string& host,
        std::uint16_t port,
        const std::string& username,
        const std::string& password);

    /**
     * @brief Logout the current user and disconnect from the server.
     *
     * Sends a logout request to mark the user as offline before disconnecting.
     */
    void disconnect();

    /**
     * @brief Logout the current user without disconnecting.
     *
     * Sends a logout request to the server to mark the user as offline.
     * @return true if logout was successful, false otherwise
     */
    bool logout();

    /**
     * @brief Check if currently connected.
     */
    bool isConnected() const;

    /**
     * @brief Send a request if connected.
     *
     * @param request The request frame to send
     * @return Response frame or error code
     */
    std::expected<comms::messaging::frame, comms::messaging::error_code>
    sendRequest(comms::messaging::frame request);

    /**
     * @brief Get the current client (internal use only).
     */
    std::shared_ptr<comms::net::client> getClient() const { return client_; }

    /**
     * @brief Get the IO context executor.
     */
    boost::asio::any_io_executor getExecutor() {
        return io_context_->get_executor();
    }

    /**
     * @brief Subscribe to server-push notifications for an event type.
     *
     * This method is non-blocking - the subscription request is sent
     * asynchronously and any errors are logged.
     *
     * @param eventType The event type to subscribe to (e.g., "ores.risk.currency_changed_event")
     */
    void subscribeToEvent(const std::string& eventType);

    /**
     * @brief Unsubscribe from server-push notifications for an event type.
     *
     * This method is non-blocking - the unsubscription request is sent
     * asynchronously and any errors are logged.
     *
     * @param eventType The event type to unsubscribe from
     */
    void unsubscribeFromEvent(const std::string& eventType);

signals:
    void connected();
    void disconnected();
    void reconnecting();
    void reconnected();
    void connectionError(const QString& message);

    /**
     * @brief Emitted when a notification is received from the server.
     *
     * @param eventType The event type name (e.g., "ores.risk.currency_changed_event")
     * @param timestamp When the event occurred
     */
    void notificationReceived(const QString& eventType, const QDateTime& timestamp);

private:
    void setupIO();
    void cleanupIO();

private:
    // Persistent IO infrastructure
    std::unique_ptr<boost::asio::io_context> io_context_;
    std::unique_ptr<boost::asio::executor_work_guard<
        boost::asio::io_context::executor_type>> work_guard_;
    std::unique_ptr<std::thread> io_thread_;

    // Transient client
    std::shared_ptr<comms::net::client> client_;

    // Remote event adapter for subscriptions
    std::unique_ptr<comms::service::remote_event_adapter> event_adapter_;

    // Logged-in account tracking
    std::optional<boost::uuids::uuid> logged_in_account_id_;

    // Event bus for publishing connection events (passed to client)
    std::shared_ptr<eventing::service::event_bus> event_bus_;

    // Connection details for event publishing
    std::string connected_host_;
    std::uint16_t connected_port_{0};
};

}

#endif
