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
#ifndef ORES_HTTP_NET_HTTP_SESSION_HPP
#define ORES_HTTP_NET_HTTP_SESSION_HPP

#include <memory>
#include <string>
#include <chrono>
#include <functional>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/awaitable.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include "ores.http/net/router.hpp"
#include "ores.http/net/http_server_options.hpp"
#include "ores.http/middleware/jwt_authenticator.hpp"
#include "ores.telemetry/log/make_logger.hpp"

namespace ores::http::net {

/**
 * @brief Callback type for updating session bytes after each request.
 *
 * Called after each authenticated request with the session ID, start time,
 * and byte counts. The callback should update the session record in the database.
 *
 * @param session_id UUID string of the session
 * @param start_time Session start time (for efficient hypertable lookup)
 * @param bytes_sent Number of bytes sent in this request's response
 * @param bytes_received Number of bytes received in this request's body
 */
using session_bytes_callback = std::function<void(
    const std::string& session_id,
    std::chrono::system_clock::time_point start_time,
    std::size_t bytes_sent,
    std::size_t bytes_received)>;

/**
 * @brief Handles a single HTTP session/connection.
 */
class http_session final : public std::enable_shared_from_this<http_session> {
public:
    explicit http_session(boost::asio::ip::tcp::socket socket,
        std::shared_ptr<router> router,
        std::shared_ptr<middleware::jwt_authenticator> authenticator,
        const http_server_options& options,
        session_bytes_callback bytes_callback = nullptr);

    /**
     * @brief Starts processing the session.
     */
    boost::asio::awaitable<void> run();

private:
    inline static std::string_view logger_name = "ores.http.net.http_session";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    boost::asio::awaitable<void> handle_request(
        boost::beast::http::request<boost::beast::http::string_body> req);

    domain::http_request convert_request(
        const boost::beast::http::request<boost::beast::http::string_body>& req);

    boost::beast::http::response<boost::beast::http::string_body> convert_response(
        const domain::http_response& resp, unsigned version, bool keep_alive);

    domain::http_method convert_method(boost::beast::http::verb verb);

    void parse_query_params(const std::string& target, domain::http_request& req);

    boost::beast::tcp_stream stream_;
    boost::beast::flat_buffer buffer_;
    std::shared_ptr<router> router_;
    std::shared_ptr<middleware::jwt_authenticator> authenticator_;
    http_server_options options_;
    std::string remote_address_;
    session_bytes_callback bytes_callback_;
};

}

#endif
