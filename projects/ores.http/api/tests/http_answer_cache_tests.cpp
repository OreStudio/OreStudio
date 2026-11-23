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
#include <thread>

using namespace ores::http::net;
using namespace ores::http::domain;
using namespace ores::logging;
namespace asio = boost::asio;
namespace beast = boost::beast;
namespace http = beast::http;

namespace {

/*
 * The names in this file are prefixed because the component's test files share
 * one translation unit, and a neighbour already owns test_server and post.
 */

/// A server on a free port, running until the test drops it.
class cache_test_server final {
public:
    explicit cache_test_server(const http_server_options& options)
        : server_(std::make_shared<http_server>(io_, options)) {
        asio::co_spawn(
            io_, [this]() -> asio::awaitable<void> { co_await server_->run(); }, asio::detached);
        thread_ = std::thread([this] { io_.run(); });

        for (auto attempt = 0; attempt < 500 && port_ == 0; ++attempt) {
            port_ = server_->bound_port();
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
    }

    ~cache_test_server() {
        server_->stop();
        io_.stop();
        if (thread_.joinable())
            thread_.join();
    }

    cache_test_server(const cache_test_server&) = delete;
    cache_test_server& operator=(const cache_test_server&) = delete;

    [[nodiscard]] http_server& server() {
        return *server_;
    }

    [[nodiscard]] std::uint16_t port() const {
        return port_;
    }

private:
    asio::io_context io_;
    std::shared_ptr<http_server> server_;
    std::thread thread_;
    std::uint16_t port_{0};
};

/// Gets @p target and returns the answer, or nothing when none arrived in time.
std::optional<http::response<http::string_body>>
cache_get(std::uint16_t port, const std::string& target, std::chrono::milliseconds budget) {
    asio::io_context io;
    beast::tcp_stream stream(io);
    beast::error_code ec;
    stream.expires_after(budget);

    const auto address = asio::ip::make_address("127.0.0.1", ec);
    if (ec)
        return std::nullopt;
    stream.connect(asio::ip::tcp::endpoint(address, port), ec);
    if (ec)
        return std::nullopt;

    http::request<http::string_body> req{http::verb::get, target, 11};
    req.set(http::field::host, "127.0.0.1");
    req.prepare_payload();
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

/// Answers a fixed JSON body, and whatever cache rule the case gives it.
request_handler cache_handler(std::optional<std::string> own_rule = std::nullopt) {
    return [own_rule](const http_request&) -> asio::awaitable<http_response> {
        auto answer = http_response::json(R"({"ok":true})");
        if (own_rule)
            answer.set_header("cache-control", *own_rule);
        co_return answer;
    };
}

http_server_options cache_options() {
    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;
    return options;
}

}

/**
 * @brief An API answer that states no cache rule is not storable.
 *
 * The BFF stamped every /api/ answer that set none with private, no-store, so a
 * refusal or a person's rows cannot be served to somebody else from a cache.
 */
TEST_CASE("an_api_answer_is_not_storable_unless_it_says_otherwise", "[http_api][cache]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that an API answer states that it is not storable";

    cache_test_server fixture(cache_options());
    auto router = fixture.server().get_router();
    router->add_route(router->get("/api/probe")
                          .summary("Answer with nothing said about caching")
                          .auth_optional()
                          .handler(cache_handler())
                          .build());

    const auto answer = cache_get(fixture.port(), "/api/probe", std::chrono::seconds(5));

    REQUIRE(answer.has_value());
    REQUIRE(answer->result_int() == 200);
    REQUIRE((*answer)[http::field::cache_control] == "private, no-store");
}

/**
 * @brief A route that states its own rule keeps it.
 *
 * The download routes and the views state a rule that is theirs, and a blanket
 * answer would take it away: a file served again is the point of that rule.
 */
TEST_CASE("a_route_that_states_its_own_cache_rule_keeps_it", "[http_api][cache]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that a route's own cache rule stands";

    cache_test_server fixture(cache_options());
    auto router = fixture.server().get_router();
    router->add_route(router->get("/api/owned")
                          .summary("Answer with its own cache rule")
                          .auth_optional()
                          .handler(cache_handler("public, max-age=60"))
                          .build());

    const auto answer = cache_get(fixture.port(), "/api/owned", std::chrono::seconds(5));

    REQUIRE(answer.has_value());
    REQUIRE((*answer)[http::field::cache_control] == "public, max-age=60");
}

/**
 * @brief A refusal for an API path carries the rule too.
 *
 * The answer is built by the session and not by a route, so this is the case that
 * proves the rule is stated for the request rather than for a handler.
 */
TEST_CASE("a_missing_api_answer_is_not_storable_either", "[http_api][cache]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that a missing API answer states that it is not storable";

    cache_test_server fixture(cache_options());
    auto router = fixture.server().get_router();
    router->add_route(router->get("/api/known")
                          .summary("A route, so the router is not empty")
                          .auth_optional()
                          .handler(cache_handler())
                          .build());

    const auto answer = cache_get(fixture.port(), "/api/unknown", std::chrono::seconds(5));

    REQUIRE(answer.has_value());
    REQUIRE(answer->result_int() == 404);
    REQUIRE((*answer)[http::field::cache_control] == "private, no-store");
}

/**
 * @brief A path outside the API keeps whatever caching it chose.
 *
 * The bundle is served from this server too, and its files have their own rule: a
 * hashed asset may be held for a year, and the index may not be held at all.
 */
TEST_CASE("a_path_outside_the_api_states_no_such_rule", "[http_api][cache]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that a non-API path states no cache rule of ours";

    cache_test_server fixture(cache_options());
    auto router = fixture.server().get_router();
    router->add_route(router->get("/probe")
                          .summary("Answer outside the API")
                          .auth_optional()
                          .handler(cache_handler())
                          .build());

    const auto answer = cache_get(fixture.port(), "/probe", std::chrono::seconds(5));

    REQUIRE(answer.has_value());
    REQUIRE(answer->result_int() == 200);
    REQUIRE((*answer)[http::field::cache_control].empty());
}
