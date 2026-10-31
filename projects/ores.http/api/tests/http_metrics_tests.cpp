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
#include "ores.http.api/domain/session_cookie.hpp"
#include "ores.http.api/domain/session_store.hpp"
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
#include <optional>
#include <string>
#include <thread>
#include <utility>
#include <vector>

using namespace ores::http::net;
using namespace ores::http::domain;
using namespace ores::logging;
namespace asio = boost::asio;
namespace beast = boost::beast;
namespace http = beast::http;

namespace {

class metrics_fixture final {
public:
    explicit metrics_fixture(const http_server_options& options)
        : server_(std::make_shared<http_server>(io_, options)) {
        asio::co_spawn(
            io_, [this]() -> asio::awaitable<void> { co_await server_->run(); }, asio::detached);
        thread_ = std::thread([this] { io_.run(); });
        for (auto attempt = 0; attempt < 500 && port_ == 0; ++attempt) {
            port_ = server_->bound_port();
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
    }

    ~metrics_fixture() {
        server_->stop();
        io_.stop();
        if (thread_.joinable())
            thread_.join();
    }

    metrics_fixture(const metrics_fixture&) = delete;
    metrics_fixture& operator=(const metrics_fixture&) = delete;

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

/// Sends one request and returns the answer, or nothing when none arrived.
std::optional<http::response<http::string_body>> metrics_send(std::uint16_t port,
                                                              http::verb verb,
                                                              const std::string& target,
                                                              const std::string& body) {
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
    req.body() = body;
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

/// Sends a request with an extra header and returns the answer.
std::optional<http::response<http::string_body>> metrics_send_with(std::uint16_t port,
                                                                   http::verb verb,
                                                                   const std::string& target,
                                                                   const std::string& cookie) {
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
    if (!cookie.empty())
        req.set(http::field::cookie, cookie);
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

/// Sends a request with a cookie and any other headers the case needs.
std::optional<http::response<http::string_body>>
metrics_send_with_headers(std::uint16_t port,
                          http::verb verb,
                          const std::string& target,
                          const std::vector<std::pair<std::string, std::string>>& headers) {
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
    for (const auto& [name, value] : headers)
        req.set(name, value);
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

/// The value of one exposition line, or zero when it is not there.
std::uint64_t metric_value(const std::string& text, const std::string& name) {
    const auto at = text.find("\n" + name + " ");
    if (at == std::string::npos)
        return 0;
    const auto value_at = at + name.size() + 2;
    const auto end = text.find('\n', value_at);
    return std::stoull(text.substr(value_at, end - value_at));
}

}

TEST_CASE("health_reports_what_the_server_can_do", "[http_metrics]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that health states the verifier's presence";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;

    metrics_fixture fixture(options);

    const auto answered = metrics_send(fixture.port(), http::verb::get, "/health", "");
    REQUIRE(answered.has_value());
    REQUIRE(answered->result_int() == 200);
    // No verifier is set on this server, so it can believe nobody.
    REQUIRE(answered->body().find(R"("verifier":"absent")") != std::string::npos);
    REQUIRE(answered->body().find(R"("status":"ready")") != std::string::npos);
}

TEST_CASE("a_session_cookie_authenticates_a_request_without_a_token",
          "[session_cookie][http_metrics]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that the cookie is a credential and not a cache";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;

    metrics_fixture fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->get("/private")
                          .summary("Something a signed-in caller may read")
                          .auth_required()
                          .handler([](const http_request& req) -> asio::awaitable<http_response> {
                              co_return http_response::json(
                                  R"({"subject":")" + req.authenticated_user->subject + R"("})");
                          })
                          .build());

    cookie_settings settings;
    settings.name = "ores_web_session_test";
    fixture.server().set_cookie_settings(settings);

    session_record record;
    record.token = "the-token";
    record.claims.subject = "the-account";
    record.claims.roles = {"reader"};
    const auto identifier = fixture.server().sessions()->open(record);
    const auto cookie_header = settings.name + "=" + identifier;

    const auto answered =
        metrics_send_with(fixture.port(), http::verb::get, "/private", cookie_header);

    REQUIRE(answered.has_value());
    REQUIRE(answered->result_int() == 200);
    REQUIRE(answered->body().find(R"("subject":"the-account")") != std::string::npos);
    // The token is the server's, and never travels in the answer.
    REQUIRE(answered->body().find("the-token") == std::string::npos);
}

TEST_CASE("a_session_the_server_does_not_know_is_refused_and_the_cookie_cleared",
          "[session_cookie][http_metrics]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a dead cookie becomes a sign-in, not a loop";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;

    metrics_fixture fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->get("/private")
                          .summary("Something a signed-in caller may read")
                          .auth_required()
                          .handler([](const http_request&) -> asio::awaitable<http_response> {
                              co_return http_response::json(R"({"ok":true})");
                          })
                          .build());

    cookie_settings settings;
    settings.name = "ores_web_session_test";
    fixture.server().set_cookie_settings(settings);

    const auto answered = metrics_send_with(
        fixture.port(), http::verb::get, "/private", settings.name + "=an-identifier-nobody-holds");

    REQUIRE(answered.has_value());
    REQUIRE(answered->result_int() == 401);
    // The browser is told to drop what it holds.
    REQUIRE((*answered)[http::field::set_cookie].find("Max-Age=0") != std::string::npos);
}

TEST_CASE("a_session_request_that_changes_something_must_prove_intent", "[session_cookie][csrf]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a cookie alone cannot change anything";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;

    metrics_fixture fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/change")
                          .summary("Something a signed-in caller may change")
                          .auth_required()
                          .handler([](const http_request&) -> asio::awaitable<http_response> {
                              co_return http_response::json(R"({"changed":true})");
                          })
                          .build());

    cookie_settings settings;
    settings.name = "ores_web_session_test";
    fixture.server().set_cookie_settings(settings);

    session_record record;
    record.token = "the-token";
    record.claims.subject = "the-account";
    const auto identifier = fixture.server().sessions()->open(record);
    const auto cookie = settings.name + "=" + identifier;

    // The cookie alone is what a cross-site form would send.
    const auto bare = metrics_send_with_headers(
        fixture.port(), http::verb::post, "/change", {{"Cookie", cookie}});
    REQUIRE(bare.has_value());
    REQUIRE(bare->result_int() == 403);
    REQUIRE(bare->body().find("X-Ores-Request") != std::string::npos);

    // The header without an Origin is still not proof.
    const auto no_origin = metrics_send_with_headers(
        fixture.port(), http::verb::post, "/change", {{"Cookie", cookie}, {"X-Ores-Request", "1"}});
    REQUIRE(no_origin.has_value());
    REQUIRE(no_origin->result_int() == 403);

    // An Origin belonging to somebody else is refused.
    const auto foreign = metrics_send_with_headers(
        fixture.port(),
        http::verb::post,
        "/change",
        {{"Cookie", cookie}, {"X-Ores-Request", "1"}, {"Origin", "https://elsewhere.example"}});
    REQUIRE(foreign.has_value());
    REQUIRE(foreign->result_int() == 403);

    // This deployment's own origin and the header are enough.
    const auto intended = metrics_send_with_headers(
        fixture.port(),
        http::verb::post,
        "/change",
        {{"Cookie", cookie}, {"X-Ores-Request", "1"}, {"Origin", "http://127.0.0.1"}});
    REQUIRE(intended.has_value());
    REQUIRE(intended->result_int() == 200);
}

TEST_CASE("metrics_count_what_the_server_answered", "[http_metrics]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an answer moves the counter it belongs to";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;

    metrics_fixture fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/echo")
                          .summary("Answer")
                          .auth_optional()
                          .handler([](const http_request& req) -> asio::awaitable<http_response> {
                              co_return http_response::json(R"({"bytes":)" +
                                                            std::to_string(req.body.size()) + "}");
                          })
                          .build());

    REQUIRE(metrics_send(fixture.port(), http::verb::post, "/echo", "hello").has_value());

    const auto scraped = metrics_send(fixture.port(), http::verb::get, "/metrics", "");
    REQUIRE(scraped.has_value());
    REQUIRE(scraped->result_int() == 200);
    REQUIRE((*scraped)[http::field::content_type].starts_with("text/plain"));

    /*
     * Two requests have been read: the echo, and the scrape itself, which is
     * counted before its answer is built. Its own answer is counted after, so the
     * response totals still show one.
     */
    REQUIRE(metric_value(scraped->body(), "ores_http_requests_total") == 2);
    REQUIRE(metric_value(scraped->body(), "ores_http_responses_2xx_total") == 1);
    REQUIRE(metric_value(scraped->body(), "ores_http_request_bytes_total") == 5);
}

TEST_CASE("a_refused_body_moves_its_own_counter", "[http_metrics]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a refusal is visible where an operator looks";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.default_max_body_size = 64;
    options.worker_threads = 2;

    metrics_fixture fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/echo")
                          .summary("Answer")
                          .auth_optional()
                          .handler([](const http_request&) -> asio::awaitable<http_response> {
                              co_return http_response::json(R"({"bytes":0})");
                          })
                          .build());

    const auto refused =
        metrics_send(fixture.port(), http::verb::post, "/echo", std::string(500, 'x'));
    REQUIRE(refused.has_value());
    REQUIRE(refused->result_int() == 413);

    const auto scraped = metrics_send(fixture.port(), http::verb::get, "/metrics", "");
    REQUIRE(scraped.has_value());
    REQUIRE(metric_value(scraped->body(), "ores_http_refused_too_large_total") == 1);
    REQUIRE(metric_value(scraped->body(), "ores_http_responses_4xx_total") == 1);
}
