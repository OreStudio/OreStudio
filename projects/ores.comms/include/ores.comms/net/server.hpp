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
#ifndef ORES_COMMS_NET_SERVER_HPP
#define ORES_COMMS_NET_SERVER_HPP

#include <list>
#include <mutex>
#include <memory>
#include <atomic>
#include <functional>
#include <string_view>
#include <boost/asio/ssl.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/cancellation_signal.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/net/server_options.hpp"
#include "ores.comms/messaging/message_dispatcher.hpp"
#include "ores.comms/messaging/message_handler.hpp"
#include "ores.comms/service/auth_session_service.hpp"

namespace ores::comms::net { class server_session; }
namespace ores::comms::service { class subscription_manager; }

namespace ores::comms::net {

using tcp = boost::asio::ip::tcp;
namespace ssl = boost::asio::ssl;

/**
 * @brief ORES protocol server.
 *
 * Accepts SSL connections, performs handshake, and manages client sessions.
 */
class server final : public std::enable_shared_from_this<server> {
private:
    inline static std::string_view logger_name =
        "ores.comms.net.server";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct server with configuration.
     *
     * @param options Server options including port, SSL config, etc.
     * @param subscription_mgr Optional subscription manager for event notifications.
     */
    explicit server(server_options options,
        std::shared_ptr<service::subscription_manager> subscription_mgr = nullptr);

    /**
     * @brief Register a message handler for a range of message types.
     *
     * Must be called before run() to register subsystem handlers.
     */
    void register_handler(messaging::message_type_range range,
        std::shared_ptr<messaging::message_handler> handler);

    /**
     * @brief Get the shared auth session service.
     *
     * Use this to pass the auth session service to handlers that need it
     * (e.g., accounts_message_handler for login/logout management).
     */
    [[nodiscard]] std::shared_ptr<service::auth_session_service> sessions() const {
        return sessions_;
    }

    /**
     * @brief Run the server.
     *
     * Accepts connections and spawns sessions until stopped.
     *
     * @param io_context The io_context to run the server on
     * @param on_listening Optional callback invoked when server starts listening
     */
    boost::asio::awaitable<void> run(boost::asio::io_context& io_context,
        std::function<void(std::uint16_t)> on_listening = nullptr);

    /**
     * @brief Stop the server.
     *
     * Stops all active sessions and the accept loop.
     */
    void stop();

    /**
     * @brief Broadcast database status to all connected clients.
     *
     * Sends a database_status_notification to all active sessions,
     * regardless of their subscription status.
     *
     * @param available Whether the database is available.
     * @param error_message Error message if unavailable, empty otherwise.
     */
    void broadcast_database_status(bool available, const std::string& error_message);

private:
    /**
     * @brief Accept connections and spawn sessions.
     */
    boost::asio::awaitable<void> accept_loop(boost::asio::io_context& io_context,
        std::function<void(std::uint16_t)> on_listening);

    /**
     * @brief Create and configure SSL context.
     */
    void setup_ssl_context();

    /**
     * @brief Watch for stop signals.
     */
    boost::asio::awaitable<void> watch_for_stop_signals(boost::asio::io_context& io_context);

    server_options options_;
    ssl::context ssl_ctx_;
    std::shared_ptr<service::auth_session_service> sessions_;
    std::shared_ptr<messaging::message_dispatcher> dispatcher_;
    std::shared_ptr<service::subscription_manager> subscription_mgr_;
    std::atomic<std::size_t> active_connections_{0};
    boost::asio::cancellation_signal stop_signal_;
    std::list<std::shared_ptr<server_session>> active_sessions_;
    std::mutex sessions_mutex_;
};

}

#endif
