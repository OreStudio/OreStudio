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
#include "ores.http/net/http_server.hpp"

#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/use_awaitable.hpp>
#include "ores.http/net/http_session.hpp"

namespace ores::http::net {

using namespace ores::telemetry::log;
namespace asio = boost::asio;
using asio::ip::tcp;

http_server::http_server(asio::io_context& io_ctx, const http_server_options& options)
    : io_ctx_(io_ctx)
    , options_(options)
    , router_(std::make_shared<router>())
    , registry_(std::make_shared<openapi::endpoint_registry>()) {

    BOOST_LOG_SEV(lg(), info) << "HTTP server initializing with options: " << options_;

    // Configure JWT authenticator if secret or public key provided
    if (!options_.jwt_secret.empty()) {
        BOOST_LOG_SEV(lg(), info) << "Configuring JWT authenticator with HS256";
        authenticator_ = std::make_shared<middleware::jwt_authenticator>(
            middleware::jwt_authenticator::create_hs256(
                options_.jwt_secret, options_.jwt_issuer, options_.jwt_audience));
    } else if (!options_.jwt_public_key_file.empty()) {
        BOOST_LOG_SEV(lg(), info) << "Configuring JWT authenticator with RS256";
        // TODO: Read public key from file
        BOOST_LOG_SEV(lg(), warn) << "RS256 from file not yet implemented";
    } else {
        BOOST_LOG_SEV(lg(), warn) << "No JWT configuration provided, "
            << "authentication will not be enforced";
    }

    setup_builtin_routes();
}

void http_server::setup_builtin_routes() {
    BOOST_LOG_SEV(lg(), debug) << "Setting up built-in routes";

    // Health check endpoint
    auto health_builder = router_->get("/health")
        .summary("Health check")
        .description("Returns server health status")
        .tags({"system"})
        .handler([](const domain::http_request&) -> asio::awaitable<domain::http_response> {
            co_return domain::http_response::json(R"({"status":"healthy"})");
        });
    router_->add_route(health_builder.build());

    // OpenAPI spec endpoint
    auto openapi_builder = router_->get("/openapi.json")
        .summary("OpenAPI specification")
        .description("Returns the OpenAPI 3.0 JSON specification")
        .tags({"system"})
        .handler([this](const domain::http_request&) -> asio::awaitable<domain::http_response> {
            auto spec = registry_->generate_openapi_json();
            co_return domain::http_response::json(spec);
        });
    router_->add_route(openapi_builder.build());

    // Swagger UI endpoint
    auto swagger_builder = router_->get("/swagger")
        .summary("Swagger UI")
        .description("Interactive API documentation")
        .tags({"system"})
        .handler([this](const domain::http_request&) -> asio::awaitable<domain::http_response> {
            auto html = registry_->generate_swagger_ui_html("/openapi.json");
            domain::http_response resp;
            resp.status = domain::http_status::ok;
            resp.content_type = "text/html";
            resp.body = html;
            co_return resp;
        });
    router_->add_route(swagger_builder.build());

    BOOST_LOG_SEV(lg(), info) << "Built-in routes registered: /health, /openapi.json, /swagger";
}

asio::awaitable<void> http_server::run() {
    BOOST_LOG_SEV(lg(), info) << "Starting HTTP server on " << options_.address
        << ":" << options_.port;

    tcp::endpoint endpoint(asio::ip::make_address(options_.address), options_.port);

    acceptor_ = std::make_unique<tcp::acceptor>(io_ctx_, endpoint);
    acceptor_->set_option(asio::socket_base::reuse_address(true));

    running_.store(true);
    BOOST_LOG_SEV(lg(), info) << "HTTP server started, accepting connections";

    co_await accept_connections();

    BOOST_LOG_SEV(lg(), info) << "HTTP server stopped";
}

asio::awaitable<void> http_server::accept_connections() {
    while (running_.load()) {
        try {
            BOOST_LOG_SEV(lg(), trace) << "Waiting for connection...";

            tcp::socket socket = co_await acceptor_->async_accept(asio::use_awaitable);

            auto remote = socket.remote_endpoint();
            BOOST_LOG_SEV(lg(), info) << "Accepted connection from "
                << remote.address().to_string() << ":" << remote.port();

            if (active_connections_.load() >= options_.max_connections) {
                BOOST_LOG_SEV(lg(), warn) << "Max connections reached ("
                    << options_.max_connections << "), rejecting connection";
                socket.close();
                continue;
            }

            active_connections_.fetch_add(1);
            BOOST_LOG_SEV(lg(), debug) << "Active connections: "
                << active_connections_.load();

            // Create and run session
            auto session = std::make_shared<http_session>(
                std::move(socket), router_, authenticator_, options_, bytes_callback_);

            asio::co_spawn(io_ctx_,
                [this, session]() -> asio::awaitable<void> {
                    try {
                        co_await session->run();
                    } catch (const std::exception& e) {
                        BOOST_LOG_SEV(lg(), error) << "Session exception: " << e.what();
                    }
                    active_connections_.fetch_sub(1);
                    BOOST_LOG_SEV(lg(), debug) << "Session ended, active connections: "
                        << active_connections_.load();
                },
                asio::detached);

        } catch (const boost::system::system_error& e) {
            if (e.code() == asio::error::operation_aborted) {
                BOOST_LOG_SEV(lg(), debug) << "Accept operation aborted";
                break;
            }
            BOOST_LOG_SEV(lg(), error) << "Accept error: " << e.what();
        }
    }
}

void http_server::stop() {
    BOOST_LOG_SEV(lg(), info) << "Stopping HTTP server...";
    running_.store(false);

    if (acceptor_ && acceptor_->is_open()) {
        boost::system::error_code ec;
        acceptor_->close(ec);
        if (ec) {
            BOOST_LOG_SEV(lg(), warn) << "Error closing acceptor: " << ec.message();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "HTTP server stop initiated, "
        << "waiting for " << active_connections_.load() << " active connections";
}

}
