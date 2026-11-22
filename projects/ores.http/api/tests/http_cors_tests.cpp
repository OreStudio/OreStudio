/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.http.api/net/cors.hpp"
#include "ores.http.api/net/http_server.hpp"
#include "ores.logging/logger.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <thread>

using namespace ores::http::net;
using namespace ores::logging;

namespace {

/// The value of @p name among @p headers, or nothing when they do not hold it.
std::string header_of(const std::vector<std::pair<std::string, std::string>>& headers,
                      std::string_view name) {
    for (const auto& [key, value] : headers)
        if (key == name)
            return value;
    return {};
}

http_server_options listing(const std::string& origins) {
    http_server_options options;
    options.cors_allowed_origins = origins;
    return options;
}

}

/**
 * @brief A wildcard deployment answers any origin, and never with credentials.
 *
 * That is what an anonymous caller needs and what a session can never use: a browser
 * refuses to send a cookie to an answer that names every origin.
 */
TEST_CASE("a_wildcard_answers_any_origin_without_credentials", "[http_api][cors]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that a wildcard deployment answers any origin";

    const auto headers = cors_headers(listing("*"), "http://localhost:20402");

    REQUIRE(header_of(headers, "Access-Control-Allow-Origin") == "*");
    REQUIRE(header_of(headers, "Access-Control-Allow-Credentials").empty());
    REQUIRE(header_of(headers, "Access-Control-Expose-Headers") ==
            "X-Request-Id, Content-Disposition");
}

/**
 * @brief A listed origin is echoed, and only a listed origin may send a session.
 *
 * The person's cookie rides the call, so the answer that admits the call must be the
 * one that names it. A cache must not answer one origin with another's answer, which
 * is what Vary states.
 */
TEST_CASE("a_listed_origin_is_echoed_and_may_carry_a_session", "[http_api][cors]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that a listed origin is echoed with credentials";

    const auto headers = cors_headers(listing("http://localhost:20401, http://localhost:20402"),
                                      "http://localhost:20402");

    REQUIRE(header_of(headers, "Access-Control-Allow-Origin") == "http://localhost:20402");
    REQUIRE(header_of(headers, "Access-Control-Allow-Credentials") == "true");
    REQUIRE(header_of(headers, "Vary") == "Origin");
}

TEST_CASE("an_origin_the_list_does_not_name_earns_nothing", "[http_api][cors]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that an unlisted origin earns no headers";

    REQUIRE(cors_headers(listing("http://localhost:20402"), "http://evil.example").empty());
}

TEST_CASE("a_request_that_states_no_origin_is_not_cross_origin", "[http_api][cors]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that a request without an origin earns no headers";

    REQUIRE(cors_headers(listing("*"), "").empty());
}

TEST_CASE("a_deployment_that_turns_cors_off_answers_none_of_it", "[http_api][cors]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that disabling CORS answers no headers";

    auto options = listing("*");
    options.enable_cors = false;

    REQUIRE(cors_headers(options, "http://localhost:20402").empty());
}

/**
 * @brief A preflight states the methods and the headers the client may send.
 *
 * =X-Ores-Request= is on the list because every write of the web client carries it and
 * the server requires it, so a preflight that omits it refuses every write.
 */
TEST_CASE("a_preflight_states_what_the_client_may_send", "[http_api][cors]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that a preflight states the allowed methods and headers";

    const auto headers =
        cors_preflight_headers(listing("http://localhost:20402"), "http://localhost:20402");

    REQUIRE(header_of(headers, "Access-Control-Allow-Methods").find("PATCH") != std::string::npos);
    REQUIRE(header_of(headers, "Access-Control-Allow-Headers").find("X-Ores-Request") !=
            std::string::npos);
    REQUIRE(header_of(headers, "Access-Control-Max-Age") == "86400");
    REQUIRE(header_of(headers, "Access-Control-Allow-Credentials") == "true");
}

TEST_CASE("a_preflight_from_an_unlisted_origin_states_no_allowance", "[http_api][cors]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that an unlisted origin earns no preflight allowance";

    REQUIRE(
        cors_preflight_headers(listing("http://localhost:20402"), "http://evil.example").empty());
}

namespace {

namespace asio = boost::asio;
namespace beast = boost::beast;
namespace http = beast::http;

/// The one origin the server below admits, which is the BFF's own.
const std::string origin_of_browser("http://localhost:20402");

/**
 * @brief Sends one request and returns the answer, or nothing when none came.
 *
 * A browser only reads an answer it is allowed to read, so the status is worth
 * nothing without the headers that admit it.
 */
std::optional<http::response<http::string_body>> send_within(std::uint16_t port,
                                                             http::verb method,
                                                             const std::string& target,
                                                             const std::string& origin) {
    asio::io_context io;
    beast::tcp_stream stream(io);
    beast::error_code ec;
    stream.expires_after(std::chrono::seconds(5));

    const auto address = asio::ip::make_address("127.0.0.1", ec);
    if (ec)
        return std::nullopt;
    stream.connect(asio::ip::tcp::endpoint(address, port), ec);
    if (ec)
        return std::nullopt;

    http::request<http::string_body> req{method, target, 11};
    req.set(http::field::host, "127.0.0.1");
    if (!origin.empty())
        req.set(http::field::origin, origin);
    http::write(stream, req, ec);
    if (ec)
        return std::nullopt;

    beast::flat_buffer buffer;
    http::response<http::string_body> res;
    http::read(stream, buffer, res, ec);
    if (ec)
        return std::nullopt;

    stream.socket().shutdown(asio::ip::tcp::socket::shutdown_both, ec);
    return res;
}

/// The value of @p name in an answer, or nothing when it does not carry it.
std::optional<std::string> answer_header(const http::response<http::string_body>& res,
                                         beast::string_view name) {
    const auto found = res.find(name);
    if (found == res.end())
        return std::nullopt;
    const auto value = found->value();
    return std::string(value.begin(), value.end());
}

}

/**
 * @brief A refusal a browser cannot read is a refusal that says nothing.
 *
 * The route that is not there, the route that refuses and the route that answers must
 * all state what a browser needs to read them, or a missing route arrives as an opaque
 * cross-origin failure and a person reads it as a broken network.
 */
TEST_CASE("a_missing_route_carries_what_a_browser_needs_to_read_it", "[http_api][cors]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that a missing route carries the CORS headers";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;
    options.cors_allowed_origins = origin_of_browser;

    asio::io_context io;
    auto server = std::make_shared<http_server>(io, options);
    asio::co_spawn(
        io, [server]() -> asio::awaitable<void> { co_await server->run(); }, asio::detached);
    std::thread io_thread([&io] { io.run(); });

    auto port = 0;
    for (auto attempt = 0; attempt < 500 && port == 0; ++attempt) {
        port = server->bound_port();
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    const auto missing = send_within(port, http::verb::get, "/no-such-route", origin_of_browser);
    const auto preflight =
        send_within(port, http::verb::options, "/no-such-route", origin_of_browser);

    server->stop();
    io.stop();
    io_thread.join();

    REQUIRE(port != 0);
    REQUIRE(missing.has_value());
    REQUIRE(missing->result_int() == 404);
    REQUIRE(answer_header(*missing, "Access-Control-Allow-Origin") == origin_of_browser);
    REQUIRE(answer_header(*missing, "Access-Control-Allow-Credentials") == std::string("true"));
    REQUIRE(answer_header(*missing, "Access-Control-Expose-Headers")->find("X-Request-Id") !=
            std::string::npos);

    // A person quotes this id from the refusal, and the session's lines hold it.
    const auto quoted = answer_header(*missing, "x-request-id");
    REQUIRE(quoted.has_value());
    REQUIRE(quoted->size() == 8);

    REQUIRE(preflight.has_value());
    REQUIRE(preflight->result_int() == 204);
    REQUIRE(answer_header(*preflight, "Access-Control-Allow-Headers")->find("X-Ores-Request") !=
            std::string::npos);
}

/**
 * @brief A page on another deployment names its own origin, and the list is what says
 * that page is ours.
 *
 * The same bundle answers on both servers, so a write reaching one of them from the
 * other is a write from a page we serve. A wildcard names none, so it still answers
 * only reads with a session.
 */
TEST_CASE("only_a_named_origin_may_be_the_source_of_a_write", "[http_api][cors]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing which origins the CSRF rule trusts";

    const auto listed = listing("http://localhost:20401, http://localhost:20402");
    REQUIRE(cors_names(listed, "http://localhost:20402"));
    REQUIRE_FALSE(cors_names(listed, "http://localhost:5173"));
    REQUIRE_FALSE(cors_names(listing("*"), "http://localhost:20402"));

    auto off = listed;
    off.enable_cors = false;
    REQUIRE_FALSE(cors_names(off, "http://localhost:20402"));
}
