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
#include "ores.http.api/net/router.hpp"
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
#include <memory>
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
 * one translation unit.
 */

/// A server on a free port, running until the test drops it.
class head_test_server final {
public:
    explicit head_test_server(const http_server_options& options)
        : server_(std::make_shared<http_server>(io_, options)) {
        asio::co_spawn(
            io_, [this]() -> asio::awaitable<void> { co_await server_->run(); }, asio::detached);
        thread_ = std::thread([this] { io_.run(); });

        for (auto attempt = 0; attempt < 500 && port_ == 0; ++attempt) {
            port_ = server_->bound_port();
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
    }

    ~head_test_server() {
        server_->stop();
        io_.stop();
        if (thread_.joinable())
            thread_.join();
    }

    head_test_server(const head_test_server&) = delete;
    head_test_server& operator=(const head_test_server&) = delete;

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
 * @brief The answer to a method and a path, and what the socket had left.
 *
 * The body of a HEAD is not read: the headers state what the GET would have
 * sent, and @c bytes_after_headers says whether any of it followed them.
 */
struct head_answer {
    unsigned status = 0;
    std::uint64_t content_length = 0;
    std::string probe_header;
    std::string body;
    std::size_t bytes_after_headers = 0;
};

std::optional<head_answer> head_send(std::uint16_t port, http::verb verb,
                                     const std::string& target) {
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

    http::request<http::string_body> req{verb, target, 11};
    req.set(http::field::host, "127.0.0.1");
    req.prepare_payload();
    http::write(stream, req, ec);
    if (ec)
        return std::nullopt;

    beast::flat_buffer buffer;
    http::response_parser<http::empty_body> parser;
    // The answer to a HEAD carries the length of a body it does not send.
    parser.skip(true);
    http::read(stream, buffer, parser, ec);
    if (ec)
        return std::nullopt;

    head_answer out;
    const auto& head = parser.get();
    out.status = head.result_int();
    const auto declared = head[http::field::content_length];
    if (!declared.empty())
        out.content_length = std::stoull(std::string(declared));
    out.probe_header = std::string(head[http::field::etag]);
    // Nothing may follow the headers: a body here would be the GET's, sent to
    // a caller that asked for the envelope only.
    out.bytes_after_headers = stream.socket().available();

    stream.socket().shutdown(asio::ip::tcp::socket::shutdown_both, ec);
    return out;
}

std::optional<std::string> head_get_body(std::uint16_t port, const std::string& target) {
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
    return res.body();
}

http_server_options head_options() {
    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;
    return options;
}

request_handler head_reader() {
    return [](const http_request&) -> asio::awaitable<http_response> {
        auto answer = http_response::json(R"({"ok":true})");
        answer.set_header("etag", "\"7\"");
        co_return answer;
    };
}

}

/**
 * @brief A HEAD answers what its GET answers, and sends no body.
 *
 * An uptime probe, a curl -I and a client that tests a resource before reading
 * it all ask this question, and the status, the headers and the length are the
 * whole answer.
 */
TEST_CASE("a_head_answers_what_its_get_answers", "[http_api][head]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that a HEAD answers what its GET answers";

    head_test_server fixture(head_options());
    auto router = fixture.server().get_router();
    router->add_route(router->get("/api/probe")
                          .summary("Answer a probe")
                          .auth_optional()
                          .handler(head_reader())
                          .build());

    const auto body = head_get_body(fixture.port(), "/api/probe");
    REQUIRE(body.has_value());

    const auto answer = head_send(fixture.port(), http::verb::head, "/api/probe");
    REQUIRE(answer.has_value());
    CHECK(answer->status == 200);
    CHECK(answer->content_length == body->size());
    CHECK(answer->probe_header == "\"7\"");
    CHECK(answer->bytes_after_headers == 0);
}

/**
 * @brief A refused read is refused over HEAD too.
 *
 * The refusal is built by the session rather than by the route, so this is the
 * case that proves HEAD is answered before a handler is reached as well as at
 * one.
 */
TEST_CASE("a_head_over_a_route_that_needs_a_session_is_refused", "[http_api][head]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that a refused read is refused over HEAD";

    head_test_server fixture(head_options());
    auto router = fixture.server().get_router();
    router->add_route(router->get("/api/private")
                          .summary("Something a signed-in caller may read")
                          .auth_required()
                          .handler(head_reader())
                          .build());

    const auto answer = head_send(fixture.port(), http::verb::head, "/api/private");
    REQUIRE(answer.has_value());
    CHECK(answer->status == 401);
    // The refusal has words for a GET; a HEAD is told how many and given none.
    CHECK(answer->content_length > 0);
    CHECK(answer->bytes_after_headers == 0);
}

/**
 * @brief A path with no GET has nothing to read, so a HEAD on it is not found.
 *
 * Answering the write route would say the path exists to a caller that asked
 * to read it, which is the opposite of what a probe is asking.
 */
TEST_CASE("a_head_on_a_path_with_no_get_is_not_found", "[http_api][head]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing that a HEAD without a GET is not found";

    head_test_server fixture(head_options());
    auto router = fixture.server().get_router();
    router->add_route(router->post("/api/writes")
                          .summary("Something a caller writes")
                          .auth_optional()
                          .no_body()
                          .handler([](const http_request&) -> asio::awaitable<http_response> {
                              co_return http_response::json(R"({"ok":true})");
                          })
                          .build());

    const auto answer = head_send(fixture.port(), http::verb::head, "/api/writes");
    REQUIRE(answer.has_value());
    CHECK(answer->status == 404);
    CHECK(answer->bytes_after_headers == 0);
}
