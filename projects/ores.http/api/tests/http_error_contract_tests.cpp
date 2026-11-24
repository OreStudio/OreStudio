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
#include "ores.http.api/domain/body_sink.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/domain/session_store.hpp"
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
#include <stdexcept>
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
class contract_test_server final {
public:
    explicit contract_test_server(const http_server_options& options)
        : server_(std::make_shared<http_server>(io_, options)) {
        asio::co_spawn(
            io_, [this]() -> asio::awaitable<void> { co_await server_->run(); }, asio::detached);
        thread_ = std::thread([this] { io_.run(); });

        for (auto attempt = 0; attempt < 500 && port_ == 0; ++attempt) {
            port_ = server_->bound_port();
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
    }

    ~contract_test_server() {
        server_->stop();
        io_.stop();
        if (thread_.joinable())
            thread_.join();
    }

    contract_test_server(const contract_test_server&) = delete;
    contract_test_server& operator=(const contract_test_server&) = delete;

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

/// A request the contract tests send, and the answer they read back.
struct contract_request {
    http::verb method = http::verb::get;
    std::string target;
    std::string body;
    std::string cookie;
};

std::optional<http::response<http::string_body>> contract_send(std::uint16_t port,
                                                               const contract_request& sent) {
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

    http::request<http::string_body> req{sent.method, sent.target, 11};
    req.set(http::field::host, "127.0.0.1");
    if (!sent.cookie.empty())
        req.set(http::field::cookie, sent.cookie);
    req.body() = sent.body;
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

http_server_options contract_options() {
    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;
    return options;
}

/**
 * @brief Checks that @p answer is a refusal the browser can read.
 *
 * The browser parses a refusal with its own schema, which states a =code= it
 * branches on and a =message= it shows. A refusal missing either is shown as
 * "Request failed with status N", which is the fault this proves against.
 */
void require_contract(const std::optional<http::response<http::string_body>>& answer,
                      unsigned status,
                      const std::string& code) {
    REQUIRE(answer.has_value());
    CHECK(answer->result_int() == status);
    CHECK((*answer)[http::field::content_type] == "application/json");

    const auto& body = answer->body();
    const auto expected_code = R"("code":")" + code + R"(")";
    INFO("body: " << body);
    CHECK(body.find(expected_code) != std::string::npos);

    const std::string message_member = R"("message":")";
    const auto at = body.find(message_member);
    REQUIRE(at != std::string::npos);
    const auto from = at + message_member.size();
    const auto to = body.rfind('"');
    REQUIRE(to > from);
    // The words that say why, which is what the browser shows.
    CHECK(to - from > 0);
}

/// A sink factory that refuses the request it is made for.
std::unique_ptr<body_sink> contract_refusing_sink(const http_request&) {
    throw std::invalid_argument("the sink does not take this request");
}

request_handler contract_writer() {
    return [](const http_request&) -> asio::awaitable<http_response> {
        co_return http_response::json(R"({"ok":true})");
    };
}

}

/**
 * @brief A route that needs a session refuses with a code and a message.
 */
TEST_CASE("a_route_that_needs_a_session_refuses_in_the_error_contract",
          "[http_api][error_contract]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the shape of a 401 from the session";

    contract_test_server fixture(contract_options());
    auto router = fixture.server().get_router();
    router->add_route(router->get("/api/private")
                          .summary("Something a signed-in caller may read")
                          .auth_required()
                          .handler(contract_writer())
                          .build());

    require_contract(
        contract_send(fixture.port(), {.target = "/api/private"}), 401, "not-authenticated");
}

/**
 * @brief A write made with a cookie must prove intent, and its refusal reads.
 *
 * The refusal is the session's own check, reached after the route matched, so
 * this proves the contract holds on the path a browser takes and not only on
 * the one a token takes.
 */
TEST_CASE("a_write_without_proof_of_intent_refuses_in_the_error_contract",
          "[http_api][error_contract]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the shape of the session's 403";

    http_server_options options = contract_options();
    contract_test_server fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/api/private")
                          .summary("Something a signed-in caller may write")
                          .auth_required()
                          .no_body()
                          .handler(contract_writer())
                          .build());

    cookie_settings settings;
    settings.name = "ores_web_session_contract_test";
    fixture.server().set_cookie_settings(settings);

    auto record = session_record{};
    const auto now = std::chrono::system_clock::now();
    record.token = "the-token";
    record.claims.subject = "the-account";
    record.claims.issued_at = now;
    record.claims.expires_at = now + std::chrono::minutes(30);
    const auto cookie = settings.name + "=" + fixture.server().sessions()->open(record);

    require_contract(
        contract_send(fixture.port(),
                      {.method = http::verb::post, .target = "/api/private", .cookie = cookie}),
        403,
        "forbidden");
}

/**
 * @brief A body the route will not take is refused by the session, and reads.
 */
TEST_CASE("a_body_the_route_will_not_take_refuses_in_the_error_contract",
          "[http_api][error_contract]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the shape of the session's 400";

    contract_test_server fixture(contract_options());
    auto router = fixture.server().get_router();
    router->add_route(router->post("/api/upload")
                          .summary("A route whose sink refuses the request")
                          .auth_optional()
                          .sink(contract_refusing_sink)
                          .handler(contract_writer())
                          .build());

    require_contract(
        contract_send(fixture.port(),
                      {.method = http::verb::post, .target = "/api/upload", .body = "{}"}),
        400,
        "invalid-request");
}

/**
 * @brief A path no route answers refuses with the contract, not with words only.
 */
TEST_CASE("a_path_no_route_answers_refuses_in_the_error_contract", "[http_api][error_contract]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the shape of the session's 404";

    contract_test_server fixture(contract_options());

    require_contract(
        contract_send(fixture.port(), {.target = "/api/nothing-answers-this"}), 404, "not-found");
}

/**
 * @brief A handler that raises is answered by the session, and the answer reads.
 */
TEST_CASE("a_handler_that_raises_refuses_in_the_error_contract", "[http_api][error_contract]") {
    auto lg(make_logger(test_suite));
    ORES_LOG_SEV(lg, info) << "Testing the shape of the session's 500";

    contract_test_server fixture(contract_options());
    auto router = fixture.server().get_router();
    router->add_route(router->get("/api/raises")
                          .summary("A handler that cannot answer")
                          .auth_optional()
                          .handler([](const http_request&) -> asio::awaitable<http_response> {
                              throw std::runtime_error("the handler could not answer");
                              co_return http_response::json(R"({})");
                          })
                          .build());

    require_contract(contract_send(fixture.port(), {.target = "/api/raises"}), 500, "internal");
}
