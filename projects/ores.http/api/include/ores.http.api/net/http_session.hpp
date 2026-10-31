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

#include "ores.http.api/domain/session_cookie.hpp"
#include "ores.http.api/domain/session_store.hpp"
#include "ores.http.api/export.hpp"
#include "ores.http.api/net/http_server_options.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/net/server_counters.hpp"
#include "ores.http.api/net/worker_pool.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <chrono>
#include <cstdint>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>

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
using session_bytes_callback = std::function<void(const std::string& session_id,
                                                  std::chrono::system_clock::time_point start_time,
                                                  std::size_t bytes_sent,
                                                  std::size_t bytes_received)>;

/**
 * @brief Handles a single HTTP session/connection.
 */
class ORES_HTTP_API_EXPORT http_session final : public std::enable_shared_from_this<http_session> {
public:
    explicit http_session(boost::asio::ip::tcp::socket socket,
                          std::shared_ptr<router> router,
                          std::shared_ptr<ores::security::jwt::jwt_authenticator> verifier,
                          std::shared_ptr<worker_pool> workers,
                          const http_server_options& options,
                          session_bytes_callback bytes_callback = nullptr,
                          server_counters* counters = nullptr,
                          std::shared_ptr<domain::session_store> sessions = nullptr,
                          domain::cookie_settings cookie = {});

    /**
     * @brief Starts processing the session.
     */
    boost::asio::awaitable<void> run();

private:
    inline static std::string_view logger_name = "ores.http.net.http_session";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    /**
     * @brief The result of reading a body: whether it arrived whole.
     */
    enum class body_outcome { complete, too_large, destination_failed };

    boost::asio::awaitable<void>
    handle_request(domain::http_request req, std::optional<domain::route> route, bool keep_alive);

    /**
     * @brief Reads the body in chunks into the sink, or into content when the
     * route takes no sink.
     *
     * The parser holds one chunk at a time, so memory is the chunk and not the
     * body. A chunk that never arrives is closed on the request timeout, so a
     * stalled upload cannot hold a connection and a worker for the life of the
     * process.
     */
    boost::asio::awaitable<body_outcome>
    read_body(boost::beast::http::request_parser<boost::beast::http::buffer_body>& parser,
              domain::body_sink* sink,
              std::string& content,
              std::uint64_t& byte_count);

    /**
     * @brief The most this request's body may be, given the route it addresses.
     *
     * The headers are read before the body so the limit can be decided before
     * any of the body is held: a route that states none gets the server's
     * default, an upload route states its own, and the server's ceiling caps
     * both.
     */
    [[nodiscard]] std::uint64_t body_limit_for(const std::optional<domain::route>& route) const;

    domain::http_request convert_request(const boost::beast::http::header<true>& head);

    boost::beast::http::response<boost::beast::http::string_body>
    convert_response(const domain::http_response& resp, unsigned version, bool keep_alive);

    /**
     * @brief Why a session request may not change anything, when it may not.
     *
     * A cookie is proof of holding a session and not of intending anything: a
     * cross-site form carries it without asking. So an unsafe method on a session
     * request must carry an =Origin= naming this deployment and a header that a form
     * cannot set, and this answers why when it does not.
     *
     * @returns nothing when the request may proceed
     */
    [[nodiscard]] std::optional<std::string> csrf_refusal(const domain::http_request& req) const;

    /**
     * @brief Writes a response, from its bytes or in chunks from its source.
     *
     * @param send_body false for a HEAD request, which answers the headers that
     *        body would have carried and none of it.
     */
    boost::asio::awaitable<void> write_response(const domain::http_response& resp,
                                                unsigned version,
                                                bool keep_alive,
                                                bool send_body);

    domain::http_method convert_method(boost::beast::http::verb verb);

    void parse_query_params(const std::string& target, domain::http_request& req);

    boost::beast::tcp_stream stream_;
    boost::beast::flat_buffer buffer_;
    std::shared_ptr<router> router_;

    // Verifies the RS256 session tokens IAM issues. A session refuses every
    // request when this is absent, rather than falling back to a second way of
    // being believed.
    std::shared_ptr<ores::security::jwt::jwt_authenticator> verifier_;

    // Runs the handler, which blocks on a downstream request. May be null, in
    // which case the handler runs on this connection's thread as before.
    std::shared_ptr<worker_pool> workers_;

    http_server_options options_;
    std::string remote_address_;
    session_bytes_callback bytes_callback_;

    // Counters the server owns and reports. Null in a test that does not want them.
    server_counters* counters_;

    // The sessions this server holds, and how it names the cookie they ride in.
    // Null when the server serves no browser session.
    std::shared_ptr<domain::session_store> sessions_;
    domain::cookie_settings cookie_;
};

}

#endif
