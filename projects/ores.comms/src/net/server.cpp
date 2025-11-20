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
#include "ores.comms/net/server.hpp"

#include <chrono>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/asio/steady_timer.hpp>
#include "ores.comms/net/session.hpp"

namespace ores::comms::net {

using namespace ores::utility::log;

server::server(server_options options)
    : options_(std::move(options)),
      ssl_ctx_(ssl::context::tlsv13),
      dispatcher_(std::make_shared<protocol::message_dispatcher>()) {
    setup_ssl_context();
}

void server::register_handler(protocol::message_type_range range,
    std::shared_ptr<protocol::message_handler> handler) {
    dispatcher_->register_handler(range, std::move(handler));
}

void server::setup_ssl_context() {
    ssl_ctx_.set_options(
        ssl::context::default_workarounds |
        ssl::context::no_sslv2 |
        ssl::context::no_sslv3 |
        ssl::context::no_tlsv1 |
        ssl::context::no_tlsv1_1 |
        ssl::context::single_dh_use);

    ssl_ctx_.use_certificate_chain_file(options_.certificate_file);
    ssl_ctx_.use_private_key_file(options_.private_key_file, ssl::context::pem);

    BOOST_LOG_SEV(lg(), info) << "SSL context configured with certificate: "
                              << options_.certificate_file;
}

boost::asio::awaitable<void> server::run(boost::asio::io_context& io_context) {
    BOOST_LOG_SEV(lg(), info) << "ORES Server starting on port " << options_.port
                              << ". Identifier: " << options_.server_identifier;
    BOOST_LOG_SEV(lg(), info) << "Protocol version: "
                              << protocol::PROTOCOL_VERSION_MAJOR << "."
                              << protocol::PROTOCOL_VERSION_MINOR;

    co_await accept_loop(io_context);
}

boost::asio::awaitable<void> server::accept_loop(boost::asio::io_context& io_context) {
    tcp::acceptor acceptor(
        co_await boost::asio::this_coro::executor,
        tcp::endpoint(tcp::v4(), options_.port));

    BOOST_LOG_SEV(lg(), info) << "Server listening on port " << options_.port;

    while (true) {
        try {
            // Wait if we've reached max connections
            while (active_connections_.load() >= options_.max_connections) {
                BOOST_LOG_SEV(lg(), info) << "Max connections (" << options_.max_connections
                                         << ") reached, waiting.";
                boost::asio::steady_timer timer(co_await boost::asio::this_coro::executor);
                timer.expires_after(std::chrono::milliseconds(100));
                co_await timer.async_wait(boost::asio::use_awaitable);
            }

            // Accept new connection
            tcp::socket socket = co_await acceptor.async_accept(
            boost::asio::use_awaitable);
            BOOST_LOG_SEV(lg(), info) << "Accepted connection from "
                                      << socket.remote_endpoint().address().to_string()
                                      << ":" << socket.remote_endpoint().port();

            // Create SSL socket and connection wrapper
            auto conn = std::make_unique<connection>(
                connection::ssl_socket(std::move(socket), ssl_ctx_));

            // Create session and spawn it
            auto sess = std::make_shared<session>(std::move(conn),
                options_.server_identifier, dispatcher_);

            // Increment active connections
            ++active_connections_;

            // Spawn session - capture shared_ptr and active_connections reference
            boost::asio::co_spawn(
                io_context,
                [sess, this]() -> boost::asio::awaitable<void> {
                    co_await sess->run();
                    --active_connections_;
                    BOOST_LOG_SEV(lg(), debug) << "Session completed, active connections: "
                                              << active_connections_.load();
                },
                boost::asio::detached);

        } catch (const boost::system::system_error& e) {
            // Check if operation was cancelled (shutdown signal)
            if (e.code() == boost::asio::error::operation_aborted) {
                BOOST_LOG_SEV(lg(), info) << "Server shutting down...";
                break;
            }
            BOOST_LOG_SEV(lg(), error) << "Accept loop error: " << e.what();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Accept loop error: " << e.what();
        }
    }
}

}
