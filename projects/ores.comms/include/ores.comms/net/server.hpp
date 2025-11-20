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
#ifndef ORES_COMMS_SERVER_HPP
#define ORES_COMMS_SERVER_HPP

#include <string>
#include <memory>
#include <atomic>
#include <boost/asio/ssl.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/net/server_options.hpp"
#include "ores.comms/protocol/message_dispatcher.hpp"
#include "ores.comms/protocol/message_handler.hpp"

namespace ores::comms::net {

using tcp = boost::asio::ip::tcp;
namespace ssl = boost::asio::ssl;

/**
 * @brief ORES protocol server.
 *
 * Accepts SSL connections, performs handshake, and manages client sessions.
 */
class server final {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger("ores.comms.server");
        return instance;
    }

public:
    /**
     * @brief Construct server with configuration.
     */
    explicit server(server_options options);

    /**
     * @brief Register a message handler for a range of message types.
     *
     * Must be called before run() to register subsystem handlers.
     */
    void register_handler(protocol::message_type_range range,
        std::shared_ptr<protocol::message_handler> handler);

    /**
     * @brief Run the server.
     *
     * Accepts connections and spawns sessions until stopped.
     *
     * @param io_context The io_context to run the server on
     */
    boost::asio::awaitable<void> run(boost::asio::io_context& io_context);

private:
    /**
     * @brief Accept connections and spawn sessions.
     */
    boost::asio::awaitable<void> accept_loop(boost::asio::io_context& io_context);

    /**
     * @brief Create and configure SSL context.
     */
    void setup_ssl_context();

    server_options options_;
    ssl::context ssl_ctx_;
    std::shared_ptr<protocol::message_dispatcher> dispatcher_;
    std::atomic<std::size_t> active_connections_{0};
};

}

#endif
