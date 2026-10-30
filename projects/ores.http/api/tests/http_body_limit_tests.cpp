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
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/http_server.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <thread>

using namespace ores::http::net;
using namespace ores::http::domain;
using namespace ores::logging;
namespace asio = boost::asio;
namespace beast = boost::beast;
namespace http = beast::http;

namespace {

/**
 * @brief A server on a free port, running until the test stops it.
 *
 * The body-limit tests all need a live server and a client that speaks to it,
 * so the start and stop live here rather than in each case.
 */
class test_server final {
public:
    explicit test_server(const http_server_options& options)
        : server_(std::make_shared<http_server>(io_, options)) {
        asio::co_spawn(
            io_, [this]() -> asio::awaitable<void> { co_await server_->run(); }, asio::detached);
        thread_ = std::thread([this] { io_.run(); });

        for (auto attempt = 0; attempt < 500 && port_ == 0; ++attempt) {
            port_ = server_->bound_port();
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
    }

    ~test_server() {
        server_->stop();
        io_.stop();
        if (thread_.joinable())
            thread_.join();
    }

    test_server(const test_server&) = delete;
    test_server& operator=(const test_server&) = delete;

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

/**
 * @brief Posts a body and returns the response, or a zero status on a timeout.
 */
http::response<http::string_body> post(std::uint16_t port,
                                       const std::string& target,
                                       const std::string& body,
                                       std::chrono::milliseconds budget) {
    asio::io_context io;
    beast::tcp_stream stream(io);
    beast::error_code ec;
    stream.expires_after(budget);

    const auto address = asio::ip::make_address("127.0.0.1", ec);
    if (ec)
        return {};
    stream.connect(asio::ip::tcp::endpoint(address, port), ec);
    if (ec)
        return {};

    http::request<http::string_body> req{http::verb::post, target, 11};
    req.set(http::field::host, "127.0.0.1");
    req.body() = body;
    req.prepare_payload();
    http::write(stream, req, ec);
    if (ec)
        return {};

    beast::flat_buffer buffer;
    http::response<http::string_body> res;
    http::read(stream, buffer, res, ec);
    if (ec)
        return {};

    stream.socket().shutdown(asio::ip::tcp::socket::shutdown_both, ec);
    return res;
}

/**
 * @brief Sends the headers of a request with an expectation and reads the reply.
 *
 * Used for the one case that must inspect an interim response, which a normal
 * request/response reader would consume as the final one.
 */
std::string expect_continue_reply(std::uint16_t port, std::uint64_t declared_body) {
    asio::io_context io;
    beast::tcp_stream stream(io);
    beast::error_code ec;
    stream.expires_after(std::chrono::seconds(5));

    const auto address = asio::ip::make_address("127.0.0.1", ec);
    if (ec)
        return {};
    stream.connect(asio::ip::tcp::endpoint(address, port), ec);
    if (ec)
        return {};

    const auto head = "POST /echo HTTP/1.1\r\nHost: 127.0.0.1\r\nContent-Length: " +
                      std::to_string(declared_body) + "\r\nExpect: 100-continue\r\n\r\n";
    asio::write(stream, asio::buffer(head), ec);
    if (ec)
        return {};

    auto buffer = std::string(64, '\0');
    const auto read = stream.read_some(asio::buffer(buffer), ec);
    if (ec)
        return {};
    buffer.resize(read);

    stream.socket().shutdown(asio::ip::tcp::socket::shutdown_both, ec);
    return buffer;
}

/// A handler that answers with the size of the body it was given.
request_handler echo_handler() {
    return [](const http_request& req) -> asio::awaitable<http_response> {
        co_return http_response::json(R"({"byte_count":)" + std::to_string(req.body.size()) + "}");
    };
}


namespace {

/// Gets one path and returns its status, or zero when it did not answer in time.
/// Distinct from the blocking suite's helper of the same shape, because both
/// files land in one translation unit.
unsigned
get_status_within(std::uint16_t port, const std::string& target, std::chrono::milliseconds budget) {
    asio::io_context io;
    beast::tcp_stream stream(io);
    beast::error_code ec;
    stream.expires_after(budget);

    const auto address = asio::ip::make_address("127.0.0.1", ec);
    if (ec)
        return 0;
    stream.connect(asio::ip::tcp::endpoint(address, port), ec);
    if (ec)
        return 0;

    http::request<http::string_body> req{http::verb::get, target, 11};
    req.set(http::field::host, "127.0.0.1");
    http::write(stream, req, ec);
    if (ec)
        return 0;

    beast::flat_buffer buffer;
    http::response<http::string_body> res;
    http::read(stream, buffer, res, ec);
    if (ec)
        return 0;

    stream.socket().shutdown(asio::ip::tcp::socket::shutdown_both, ec);
    return res.result_int();
}

}

}

TEST_CASE("a_body_past_the_default_limit_is_refused_with_413", "[http_body_limit]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a route refuses a body past the default limit";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.default_max_body_size = 64;
    options.worker_threads = 2;

    test_server fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/echo")
                          .summary("Echo the size of the body")
                          .auth_optional()
                          .handler(echo_handler())
                          .build());

    const auto refused =
        post(fixture.port(), "/echo", std::string(500, 'x'), std::chrono::seconds(5));

    REQUIRE(refused.result_int() == 413);
    // The connection closes with the refusal, because the body was not read.
    REQUIRE(refused[http::field::connection] == "close");
}

TEST_CASE("a_route_that_raises_its_limit_accepts_the_body", "[http_body_limit]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a route's own limit is the one applied";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.default_max_body_size = 64;
    options.worker_threads = 2;

    test_server fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/upload")
                          .summary("Take an upload")
                          .auth_optional()
                          .max_body(4096)
                          .handler(echo_handler())
                          .build());

    const auto accepted =
        post(fixture.port(), "/upload", std::string(500, 'x'), std::chrono::seconds(5));

    REQUIRE(accepted.result_int() == 200);
    REQUIRE(accepted.body() == R"({"byte_count":500})");
}

TEST_CASE("an_expect_continue_is_answered_before_the_body", "[http_body_limit]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that the server tells a client to send, or not";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;

    test_server fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/echo")
                          .summary("Echo the size of the body")
                          .auth_optional()
                          .handler(echo_handler())
                          .build());

    const auto reply = expect_continue_reply(fixture.port(), 16);

    REQUIRE(reply.rfind("HTTP/1.1 100 Continue", 0) == 0);
}

TEST_CASE("an_over_limit_expect_is_refused_without_inviting_the_body", "[http_body_limit]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a client is not told to send a body that is refused";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.default_max_body_size = 64;

    test_server fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/echo")
                          .summary("Echo the size of the body")
                          .auth_optional()
                          .handler(echo_handler())
                          .build());

    const auto reply = expect_continue_reply(fixture.port(), 5000);

    REQUIRE(reply.rfind("HTTP/1.1 413", 0) == 0);
}

TEST_CASE("a_saturated_pool_refuses_with_503", "[http_body_limit][worker_pool]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a full pool refuses rather than queueing";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 1;
    options.max_inflight_requests = 1;

    test_server fixture(options);
    auto router = fixture.server().get_router();

    std::atomic<bool> entered{false};
    std::atomic<bool> release{false};
    router->add_route(
        router->get("/slow")
            .summary("Block the only worker")
            .auth_optional()
            .handler([&entered, &release](const http_request&) -> asio::awaitable<http_response> {
                entered = true;
                while (!release.load())
                    std::this_thread::sleep_for(std::chrono::milliseconds(1));
                co_return http_response::json(R"({"status":"slow"})");
            })
            .build());

    std::atomic<unsigned> slow_status{0};
    std::thread slow(
        [&] { slow_status = get_status_within(fixture.port(), "/slow", std::chrono::seconds(5)); });
    for (auto waited = 0; waited < 500 && !entered.load(); ++waited)
        std::this_thread::sleep_for(std::chrono::milliseconds(10));

    const auto refused =
        get_status_within(fixture.port(), "/health", std::chrono::milliseconds(2000));

    release = true;
    slow.join();

    REQUIRE(entered.load());
    REQUIRE(slow_status.load() == 200);
    REQUIRE(refused == 503);
}
