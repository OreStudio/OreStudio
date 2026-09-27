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

#include "ores.http.api/export.hpp"
#include "ores.http.api/net/http_server_options.hpp"
#include "ores.http.api/net/http_session.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <atomic>
#include <memory>

namespace ores::http::net {

/**
 * @brief HTTP server built on Boost.Beast.
 */
class ORES_HTTP_API_EXPORT http_server final {
public:
    explicit http_server(boost::asio::io_context& io_ctx, const http_server_options& options);

    /**
     * @brief Returns the router for registering endpoints.
     */
    std::shared_ptr<router> get_router() {
        return router_;
    }

    /**
     * @brief Returns the endpoint registry for OpenAPI generation.
     */
    std::shared_ptr<openapi::endpoint_registry> get_registry() {
        return registry_;
    }

    /**
     * @brief Returns the verifier every session authenticates its caller with.
     */
    std::shared_ptr<ores::security::jwt::jwt_authenticator> get_verifier() {
        return verifier_;
    }

    /**
     * @brief Sets the verifier every session authenticates its caller with.
     *
     * Session tokens are RS256 and IAM signs them, so the server checks them
     * against IAM's public key and holds no signing key of its own. The
     * framework fetches that key at startup; this is where it reaches the
     * HTTP surface. A session is built with whatever stands here, so it is
     * set before the server starts accepting.
     */
    void set_verifier(
        std::shared_ptr<ores::security::jwt::jwt_authenticator> verifier) {
        verifier_ = std::move(verifier);
    }

    /**
     * @brief Sets the session bytes callback for tracking request/response sizes.
     *
     * This callback is invoked after each authenticated request to update
     * the session's byte counters in the database.
     */
    void set_session_bytes_callback(session_bytes_callback callback) {
        bytes_callback_ = std::move(callback);
    }

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
    bool is_running() const {
        return running_.load();
    }

private:
    inline static std::string_view logger_name = "ores.http.net.http_server";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    void setup_builtin_routes();

    boost::asio::awaitable<void> accept_connections();

    boost::asio::io_context& io_ctx_;
    http_server_options options_;
    std::shared_ptr<router> router_;

    // Verifies the RS256 session tokens IAM issues. The server holds no other
    // credential: it never mints, so a symmetric key here would be a second
    // way to be believed rather than a second way to sign.
    std::shared_ptr<ores::security::jwt::jwt_authenticator> verifier_;
    std::shared_ptr<openapi::endpoint_registry> registry_;
    std::unique_ptr<boost::asio::ip::tcp::acceptor> acceptor_;
    std::atomic<bool> running_{false};
    std::atomic<std::uint32_t> active_connections_{0};
    session_bytes_callback bytes_callback_;
};

}

#endif
