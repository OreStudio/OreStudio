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
#ifndef ORES_HTTP_NET_HTTP_SERVER_HPP
#define ORES_HTTP_NET_HTTP_SERVER_HPP

#include <memory>
#include <atomic>
#include <boost/asio/io_context.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/awaitable.hpp>
#include "ores.http/net/router.hpp"
#include "ores.http/net/http_server_options.hpp"
#include "ores.http/middleware/jwt_authenticator.hpp"
#include "ores.http/openapi/endpoint_registry.hpp"
#include "ores.telemetry/log/make_logger.hpp"

namespace ores::http::net {

/**
 * @brief HTTP server built on Boost.Beast.
 */
class http_server final {
public:
    explicit http_server(boost::asio::io_context& io_ctx,
        const http_server_options& options);

    /**
     * @brief Returns the router for registering endpoints.
     */
    std::shared_ptr<router> get_router() { return router_; }

    /**
     * @brief Returns the endpoint registry for OpenAPI generation.
     */
    std::shared_ptr<openapi::endpoint_registry> get_registry() { return registry_; }

    /**
     * @brief Starts the server and accepts connections.
     */
    boost::asio::awaitable<void> run();

    /**
     * @brief Signals the server to stop accepting new connections.
     */
    void stop();

    /**
     * @brief Returns whether the server is running.
     */
    bool is_running() const { return running_.load(); }

private:
    inline static std::string_view logger_name = "ores.http.net.http_server";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    void setup_builtin_routes();

    boost::asio::awaitable<void> accept_connections();

    boost::asio::io_context& io_ctx_;
    http_server_options options_;
    std::shared_ptr<router> router_;
    std::shared_ptr<middleware::jwt_authenticator> authenticator_;
    std::shared_ptr<openapi::endpoint_registry> registry_;
    std::unique_ptr<boost::asio::ip::tcp::acceptor> acceptor_;
    std::atomic<bool> running_{false};
    std::atomic<std::uint32_t> active_connections_{0};
};

}

#endif
