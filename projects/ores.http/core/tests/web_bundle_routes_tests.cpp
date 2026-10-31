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
#include "ores.http.core/routes/web_bundle_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <optional>
#include <string>
#include <thread>

using namespace ores::http::net;
using namespace ores::http::server::routes;
using namespace ores::logging;
namespace asio = boost::asio;
namespace beast = boost::beast;
namespace http = beast::http;
namespace fs = std::filesystem;

namespace {

/**
 * @brief The bundle a build would have written, made by hand in a temporary root.
 */
class built_bundle final {
public:
    built_bundle() {
        const auto stamp = std::chrono::steady_clock::now().time_since_epoch().count();
        root_ = fs::temp_directory_path() / ("ores-bundle-" + std::to_string(stamp));
        fs::create_directories(root_ / "assets");

        write("index.html", "<!doctype html><title>the app</title>");
        write("assets/app-ABCD1234.js", "console.log('the app');");
        write("assets/app-ABCD1234.css", "body { color: black; }");
        write("assets/logo-XYZ98765.svg", "<svg></svg>");
        write("favicon.ico", "ico");
        // A prebuilt brotli copy beside one asset.
        write("assets/app-ABCD1234.js.br", "brotli-bytes");
    }

    ~built_bundle() {
        std::error_code ec;
        fs::remove_all(root_, ec);
    }

    built_bundle(const built_bundle&) = delete;
    built_bundle& operator=(const built_bundle&) = delete;

    [[nodiscard]] const fs::path& root() const {
        return root_;
    }

private:
    void write(const std::string& path, const std::string& content) {
        std::ofstream out(root_ / path, std::ios::binary | std::ios::trunc);
        out << content;
    }

    fs::path root_;
};

/**
 * @brief A gateway that serves only the bundle.
 */
class bundle_gateway final {
public:
    explicit bundle_gateway(const fs::path& root)
        : server_(std::make_shared<http_server>(io_, make_options()))
        , bundle_(std::make_shared<web_bundle_routes>(root)) {

        bundle_->register_routes(server_->get_router(), server_->get_registry());

        asio::co_spawn(
            io_, [this]() -> asio::awaitable<void> { co_await server_->run(); }, asio::detached);
        thread_ = std::thread([this] { io_.run(); });

        for (auto attempt = 0; attempt < 500 && port_ == 0; ++attempt) {
            port_ = server_->bound_port();
            std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
    }

    ~bundle_gateway() {
        server_->stop();
        io_.stop();
        if (thread_.joinable())
            thread_.join();
    }

    bundle_gateway(const bundle_gateway&) = delete;
    bundle_gateway& operator=(const bundle_gateway&) = delete;

    [[nodiscard]] std::uint16_t port() const {
        return port_;
    }

private:
    static http_server_options make_options() {
        http_server_options options;
        options.address = "127.0.0.1";
        options.port = 0;
        options.worker_threads = 2;
        return options;
    }

    asio::io_context io_;
    std::shared_ptr<http_server> server_;
    std::shared_ptr<web_bundle_routes> bundle_;
    std::thread thread_;
    std::uint16_t port_{0};
};

/**
 * @brief Fetches a path, with an optional Accept-Encoding, and returns the answer.
 */
std::optional<http::response<http::string_body>>
fetch_bundle(std::uint16_t port, const std::string& target, const std::string& accept_encoding) {
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
    if (!accept_encoding.empty())
        req.set(http::field::accept_encoding, accept_encoding);
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

}

TEST_CASE("the_bundle_is_served_with_the_headers_its_files_deserve", "[web_bundle]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the cache headers of the application and its assets";

    built_bundle bundle;
    bundle_gateway gateway(bundle.root());

    const auto root = fetch_bundle(gateway.port(), "/", "");
    REQUIRE(root.has_value());
    REQUIRE(root->result_int() == 200);
    REQUIRE(root->body() == "<!doctype html><title>the app</title>");
    REQUIRE((*root)[http::field::content_type].starts_with("text/html"));
    REQUIRE((*root)[http::field::cache_control] == "no-cache");

    const auto asset = fetch_bundle(gateway.port(), "/assets/app-ABCD1234.js", "");
    REQUIRE(asset.has_value());
    REQUIRE(asset->result_int() == 200);
    REQUIRE(asset->body() == "console.log('the app');");
    REQUIRE((*asset)[http::field::content_type].starts_with("text/javascript"));
    REQUIRE((*asset)[http::field::cache_control].find("immutable") != std::string::npos);
}

TEST_CASE("a_screen_path_is_answered_with_the_application", "[web_bundle]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the history fallback, deep in the application";

    built_bundle bundle;
    bundle_gateway gateway(bundle.root());

    for (const auto* path : {"/refdata", "/refdata/currencies", "/refdata/currencies/123"}) {
        const auto answered = fetch_bundle(gateway.port(), path, "");
        REQUIRE(answered.has_value());
        REQUIRE(answered->result_int() == 200);
        REQUIRE(answered->body() == "<!doctype html><title>the app</title>");
    }
}

TEST_CASE("a_missing_asset_and_an_api_path_are_not_the_application", "[web_bundle]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a breakage stays visible";

    built_bundle bundle;
    bundle_gateway gateway(bundle.root());

    for (const auto* path : {"/assets/gone-00000000.js", "/api/v1/nothing", "/api"}) {
        const auto answered = fetch_bundle(gateway.port(), path, "");
        REQUIRE(answered.has_value());
        REQUIRE(answered->result_int() == 404);
        REQUIRE(answered->body().find("<!doctype html>") == std::string::npos);
    }
}

TEST_CASE("a_path_that_would_leave_the_bundle_finds_nothing", "[web_bundle]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that the manifest is the only way to a file";

    built_bundle bundle;
    bundle_gateway gateway(bundle.root());

    const auto answered = fetch_bundle(gateway.port(), "/../../etc/passwd", "");
    REQUIRE(answered.has_value());
    // The application shell, because nothing joins the path to the root.
    REQUIRE(answered->result_int() == 200);
    REQUIRE(answered->body() == "<!doctype html><title>the app</title>");
}

TEST_CASE("a_prebuilt_brotli_copy_is_sent_when_the_client_offers_it", "[web_bundle]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing compression at build time, not at request time";

    built_bundle bundle;
    bundle_gateway gateway(bundle.root());

    const auto compressed = fetch_bundle(gateway.port(), "/assets/app-ABCD1234.js", "br");
    REQUIRE(compressed.has_value());
    REQUIRE(compressed->result_int() == 200);
    REQUIRE((*compressed)[http::field::content_encoding] == "br");
    REQUIRE((*compressed)[http::field::vary].find("Accept-Encoding") != std::string::npos);
    REQUIRE(compressed->body() == "brotli-bytes");

    const auto plain = fetch_bundle(gateway.port(), "/assets/app-ABCD1234.js", "identity");
    REQUIRE(plain.has_value());
    REQUIRE((*plain)[http::field::content_encoding].empty());
    REQUIRE(plain->body() == "console.log('the app');");
}
