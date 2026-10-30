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
#include <memory>
#include <optional>
#include <sstream>
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
 * @brief What a recording sink was given, and what it was asked to do with it.
 *
 * Shared with the handler, which runs on a worker, so both sides read the same
 * record once the request is answered.
 */
struct streamed_sink_record final {
    std::string bytes;
    std::size_t chunks = 0;
    /// The session thread sets these after the response, so they are atomic and
    /// the assertions wait for them rather than reading them the instant the
    /// client sees the answer.
    std::atomic<bool> finished{false};
    std::atomic<bool> published{false};
    std::atomic<bool> discarded{false};
    /// Set before the request, to make the destination refuse the first chunk.
    bool refuse_chunks = false;
    /// Set before the request, to make the bytes fail to complete.
    bool refuse_finish = false;
};

/**
 * @brief A sink that keeps everything and records what happened to it.
 */
class streamed_recording_sink final : public body_sink {
public:
    explicit streamed_recording_sink(std::shared_ptr<streamed_sink_record> record)
        : record_(std::move(record)) {}

protected:
    bool consume_chunk(std::string_view chunk) override {
        if (record_->refuse_chunks)
            return false;
        record_->bytes.append(chunk);
        ++record_->chunks;
        return true;
    }

    bool finish_body() override {
        record_->finished = true;
        return !record_->refuse_finish;
    }

    bool publish_chunk() override {
        record_->published = true;
        return true;
    }

    void discard() noexcept override {
        record_->discarded.store(true);
    }

private:
    std::shared_ptr<streamed_sink_record> record_;
};

/**
 * @brief A server on a free port that runs until the test stops it.
 */
class streaming_fixture final {
public:
    explicit streaming_fixture(const http_server_options& options)
        : server_(std::make_shared<http_server>(io_, options)) {
        asio::co_spawn(
            io_, [this]() -> asio::awaitable<void> { co_await server_->run(); }, asio::detached);
        thread_ = std::thread([this] { io_.run(); });

        for (auto attempt = 0; attempt < 500 && port_ == 0; ++attempt) {
            port_ = server_->bound_port();
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
    }

    ~streaming_fixture() {
        server_->stop();
        io_.stop();
        if (thread_.joinable())
            thread_.join();
    }

    streaming_fixture(const streaming_fixture&) = delete;
    streaming_fixture& operator=(const streaming_fixture&) = delete;

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
 * @brief Posts a body and returns the response, or nothing when none arrived.
 */
std::optional<http::response<http::string_body>> post_streamed(std::uint16_t port,
                                                               const std::string& target,
                                                               const std::string& body,
                                                               std::chrono::milliseconds budget) {
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

    http::request<http::string_body> req{http::verb::post, target, 11};
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

/**
 * @brief Posts a chunked body in pieces, so the limit trips after some are taken.
 *
 * The frames are written by hand because Beast sends a string body as one chunk,
 * and a single chunk past the limit is refused by its header before any of it is
 * taken, which is a different case.
 */
std::optional<http::response<http::string_body>>
post_chunked_in_pieces(std::uint16_t port,
                       const std::string& target,
                       std::size_t total,
                       std::size_t piece,
                       std::chrono::milliseconds budget) {
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

    auto request =
        "POST " + target + " HTTP/1.1\r\nHost: 127.0.0.1\r\nTransfer-Encoding: chunked\r\n\r\n";
    const auto payload = std::string(piece, 'x');
    for (auto written = std::size_t{0}; written < total; written += piece) {
        std::ostringstream size;
        size << std::hex << piece << "\r\n";
        request += size.str() + payload + "\r\n";
    }
    request += "0\r\n\r\n";

    asio::write(stream, asio::buffer(request), ec);
    // A refusal closes the connection while the frames are still arriving, and the
    // answer is what matters, so a failed write is not the end of the test.
    ec = {};

    beast::flat_buffer buffer;
    http::response<http::string_body> res;
    http::read(stream, buffer, res, ec);
    if (ec)
        return std::nullopt;

    stream.socket().shutdown(asio::ip::tcp::socket::shutdown_both, ec);
    return res;
}

/**
 * @brief What became of a connection that declared a body and then went quiet.
 */
enum class stall_outcome { closed, answered, held };

/**
 * @brief Sends headers that declare a body, sends none of it, and reads.
 *
 * The server must close the connection on its own deadline rather than hold it.
 * A read that times out means the server was still waiting, which is the failure.
 */
stall_outcome
stall_after_headers(std::uint16_t port, std::uint64_t declared, std::chrono::milliseconds wait) {
    asio::io_context io;
    beast::tcp_stream stream(io);
    beast::error_code ec;
    stream.expires_after(wait);

    const auto address = asio::ip::make_address("127.0.0.1", ec);
    if (ec)
        return stall_outcome::held;
    stream.connect(asio::ip::tcp::endpoint(address, port), ec);
    if (ec)
        return stall_outcome::held;

    const auto head =
        "POST /stream HTTP/1.1\r\nHost: 127.0.0.1\r\nContent-Length: " + std::to_string(declared) +
        "\r\n\r\n";
    asio::write(stream, asio::buffer(head), ec);
    if (ec)
        return stall_outcome::held;

    beast::flat_buffer buffer;
    http::response<http::string_body> res;
    http::read(stream, buffer, res, ec);
    if (!ec)
        return stall_outcome::answered;
    if (ec == beast::error::timeout)
        return stall_outcome::held;
    return stall_outcome::closed;
}

/**
 * @brief Waits for a flag the session sets after it has answered.
 */
bool wait_until(const std::atomic<bool>& flag, std::chrono::milliseconds budget) {
    const auto deadline = std::chrono::steady_clock::now() + budget;
    while (!flag.load() && std::chrono::steady_clock::now() < deadline)
        std::this_thread::sleep_for(std::chrono::milliseconds(2));
    return flag.load();
}

/// A handler that publishes the sink and reports what it received.
request_handler streamed_echo(const std::shared_ptr<streamed_sink_record>& record) {
    return [record](const http_request& req) -> asio::awaitable<http_response> {
        if (req.sink == nullptr)
            co_return http_response::error(http_status::internal_server_error,
                                           "The route has no sink.");
        if (!req.sink->publish())
            co_return http_response::error(http_status::internal_server_error,
                                           "The bytes could not be published.");
        co_return http_response::json(R"({"bytes":)" + std::to_string(req.body_bytes) +
                                      R"(,"chunks":)" + std::to_string(record->chunks) + "}");
    };
}

}

TEST_CASE("a_route_with_a_sink_receives_the_body_in_chunks", "[http_streamed_body]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a streamed body reaches its sink whole";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;

    auto record = std::make_shared<streamed_sink_record>();
    streaming_fixture fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/stream")
                          .summary("Take a streamed body")
                          .auth_optional()
                          .max_body(1024 * 1024)
                          .sink([record](const http_request&) {
                              return std::make_unique<streamed_recording_sink>(record);
                          })
                          .handler(streamed_echo(record))
                          .build());

    const auto body = std::string(200 * 1024, 'x');
    const auto answered = post_streamed(fixture.port(), "/stream", body, std::chrono::seconds(5));

    REQUIRE(answered.has_value());
    REQUIRE(answered->result_int() == 200);
    REQUIRE(record->bytes == body);
    REQUIRE(record->bytes.size() == 200 * 1024);
    // A body larger than one chunk arrives in more than one piece.
    REQUIRE(record->chunks > 1);
    REQUIRE(record->finished.load());
    REQUIRE(record->published.load());
    REQUIRE(!record->discarded.load());
}

TEST_CASE("a_streamed_body_past_the_limit_is_refused", "[http_streamed_body]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a streamed body keeps the route's limit";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;

    auto record = std::make_shared<streamed_sink_record>();
    streaming_fixture fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/stream")
                          .summary("Take a streamed body")
                          .auth_optional()
                          .max_body(1024)
                          .sink([record](const http_request&) {
                              return std::make_unique<streamed_recording_sink>(record);
                          })
                          .handler(streamed_echo(record))
                          .build());

    const auto refused =
        post_streamed(fixture.port(), "/stream", std::string(4096, 'x'), std::chrono::seconds(5));

    REQUIRE(refused.has_value());
    REQUIRE(refused->result_int() == 413);
    /*
     * The declared size is refused before a sink exists, so nothing was taken and
     * nothing has to be discarded.
     */
    REQUIRE(record->chunks == 0);
    REQUIRE(record->bytes.empty());
    REQUIRE(!record->published.load());
}

TEST_CASE("a_stream_that_runs_past_the_limit_is_refused_and_discarded", "[http_streamed_body]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a chunked body past the limit is discarded";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;

    auto record = std::make_shared<streamed_sink_record>();
    streaming_fixture fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/stream")
                          .summary("Take a streamed body")
                          .auth_optional()
                          .max_body(4096)
                          .sink([record](const http_request&) {
                              return std::make_unique<streamed_recording_sink>(record);
                          })
                          .handler(streamed_echo(record))
                          .build());

    const auto refused =
        post_chunked_in_pieces(fixture.port(), "/stream", 8 * 1024, 1024, std::chrono::seconds(5));

    REQUIRE(refused.has_value());
    REQUIRE(refused->result_int() == 413);
    /*
     * The sink took the first chunks. Nothing publishes them, and the sink's
     * destructor is what discards them.
     */
    REQUIRE(record->chunks > 0);
    REQUIRE(wait_until(record->discarded, std::chrono::seconds(2)));
    REQUIRE(!record->published.load());
}

TEST_CASE("a_destination_that_refuses_a_chunk_is_answered_500", "[http_streamed_body]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a sink which cannot take the bytes is a 500";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;

    auto record = std::make_shared<streamed_sink_record>();
    record->refuse_chunks = true;
    streaming_fixture fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/stream")
                          .summary("Take a streamed body")
                          .auth_optional()
                          .max_body(1024 * 1024)
                          .sink([record](const http_request&) {
                              return std::make_unique<streamed_recording_sink>(record);
                          })
                          .handler(streamed_echo(record))
                          .build());

    const auto failed =
        post_streamed(fixture.port(), "/stream", std::string(4096, 'x'), std::chrono::seconds(5));

    REQUIRE(failed.has_value());
    REQUIRE(failed->result_int() == 500);
    REQUIRE(wait_until(record->discarded, std::chrono::seconds(2)));
    REQUIRE(!record->published.load());
}

TEST_CASE("a_body_that_stops_arriving_is_closed_on_the_deadline", "[http_streamed_body]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a stalled body does not hold the connection";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;
    options.request_timeout = std::chrono::seconds(1);

    auto record = std::make_shared<streamed_sink_record>();
    streaming_fixture fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/stream")
                          .summary("Take a streamed body")
                          .auth_optional()
                          .max_body(1024 * 1024)
                          .sink([record](const http_request&) {
                              return std::make_unique<streamed_recording_sink>(record);
                          })
                          .handler(streamed_echo(record))
                          .build());

    // The body is declared and never sent, so the read waits for a chunk that
    // does not come. The read budget is longer than the server's deadline, so a
    // timeout here would mean the server was still waiting.
    const auto outcome = stall_after_headers(fixture.port(), 100000, std::chrono::seconds(5));

    REQUIRE(outcome == stall_outcome::closed);
    REQUIRE(wait_until(record->discarded, std::chrono::seconds(2)));
    REQUIRE(!record->published.load());
}

TEST_CASE("bytes_that_cannot_be_completed_are_answered_500", "[http_streamed_body]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a sink which cannot finish is a 500";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;

    auto record = std::make_shared<streamed_sink_record>();
    record->refuse_finish = true;
    streaming_fixture fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/stream")
                          .summary("Take a streamed body")
                          .auth_optional()
                          .max_body(1024 * 1024)
                          .sink([record](const http_request&) {
                              return std::make_unique<streamed_recording_sink>(record);
                          })
                          .handler(streamed_echo(record))
                          .build());

    const auto failed =
        post_streamed(fixture.port(), "/stream", std::string(4096, 'x'), std::chrono::seconds(5));

    REQUIRE(failed.has_value());
    REQUIRE(failed->result_int() == 500);
    REQUIRE(wait_until(record->discarded, std::chrono::seconds(2)));
    REQUIRE(!record->published.load());
}

TEST_CASE("a_route_without_a_sink_still_receives_its_whole_body", "[http_streamed_body]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a buffered route is untouched by the sink path";

    http_server_options options;
    options.address = "127.0.0.1";
    options.port = 0;
    options.worker_threads = 2;

    streaming_fixture fixture(options);
    auto router = fixture.server().get_router();
    router->add_route(router->post("/buffered")
                          .summary("Take a buffered body")
                          .auth_optional()
                          .handler([](const http_request& req) -> asio::awaitable<http_response> {
                              co_return http_response::json(
                                  R"({"bytes":)" + std::to_string(req.body.size()) + R"(,"sink":)" +
                                  (req.sink == nullptr ? "false" : "true") + "}");
                          })
                          .build());

    const auto answered =
        post_streamed(fixture.port(), "/buffered", std::string(4096, 'x'), std::chrono::seconds(5));

    REQUIRE(answered.has_value());
    REQUIRE(answered->result_int() == 200);
    REQUIRE(answered->body() == R"({"bytes":4096,"sink":false})");
}
