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

#include <memory>
#include <string>
#include <thread>
#include <boost/asio/io_context.hpp>
#include <boost/asio/executor_work_guard.hpp>
#include <QObject>
#include "ores.comms/net/client.hpp"
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
    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.qt.client_manager");
        return instance;
    }

public:
    explicit ClientManager(QObject* parent = nullptr);
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
     * @brief Disconnect from the server.
     */
    void disconnect();

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

signals:
    void connected();
    void disconnected();
    void connectionError(const QString& message);

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
};

}

#endif
