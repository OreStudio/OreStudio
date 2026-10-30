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
#include <atomic>
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
 * @brief Gets one path and returns the status, or zero when it did not answer.
 *
 * The deadline is what the test asserts on: a server that is still answering
 * returns well inside it, and a server whose only thread is blocked does not
 * answer at all.
 */
unsigned
get_within(std::uint16_t port, const std::string& target, std::chrono::milliseconds budget) {
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

/**
 * @brief A blocked handler must not stop the server answering anything else.
 *
 * This is the defect increment A1 removes. The handler below blocks until the
 * test releases it, which is what a route does when the service behind it is
 * slow, and /health is the thing a person, systemd or a container probe watches
 * while it happens.
 */
TEST_CASE("a_blocked_handler_does_not_stop_health_answering", "[http_server][worker_pool]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a blocked handler leaves the server answering";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;
    options.max_inflight_requests = 8;

    asio::io_context io;
    auto server = std::make_shared<http_server>(io, options);

    std::atomic<bool> handler_entered{false};
    std::atomic<bool> release_handler{false};

    auto router = server->get_router();
    router->add_route(router->get("/slow")
                          .summary("A handler that blocks like a slow service")
                          .auth_optional()
                          .handler([&handler_entered, &release_handler](
                                       const http_request&) -> asio::awaitable<http_response> {
                              handler_entered = true;
                              while (!release_handler.load())
                                  std::this_thread::sleep_for(std::chrono::milliseconds(1));
                              co_return http_response::json(R"({"status":"slow"})");
                          })
                          .build());

    asio::co_spawn(
        io, [server]() -> asio::awaitable<void> { co_await server->run(); }, asio::detached);

    std::thread io_thread([&io] { io.run(); });

    // Wait for the acceptor to bind, which is when the port becomes readable.
    auto port = 0;
    for (auto attempt = 0; attempt < 500 && port == 0; ++attempt) {
        port = server->bound_port();
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }

    // Start the slow request and wait until its handler is running.
    std::atomic<unsigned> slow_status{0};
    std::thread slow_client(
        [&] { slow_status = get_within(port, "/slow", std::chrono::seconds(5)); });
    auto entered = false;
    for (auto waited = 0; waited < 500 && !handler_entered.load(); ++waited)
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    entered = handler_entered.load();

    // The handler is now blocking one worker. Health must answer anyway.
    const auto health = get_within(port, "/health", std::chrono::milliseconds(500));

    release_handler = true;
    slow_client.join();
    server->stop();
    io.stop();
    io_thread.join();

    REQUIRE(port != 0);
    REQUIRE(entered);
    REQUIRE(health == 200);
    REQUIRE(slow_status.load() == 200);
}
