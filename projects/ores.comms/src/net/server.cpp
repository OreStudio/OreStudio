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
#include <boost/asio/bind_cancellation_slot.hpp>
#include <boost/asio/signal_set.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/asio/steady_timer.hpp>
#include "ores.comms/net/session.hpp"
#include "ores.comms/service/subscription_manager.hpp"

namespace ores::comms::net {

using namespace ores::utility::log;

server::server(server_options options,
    std::shared_ptr<service::subscription_manager> subscription_mgr)
    : options_(std::move(options)),
      ssl_ctx_(ssl::context::tlsv13),
      sessions_(std::make_shared<service::session_service>()),
      dispatcher_(std::make_shared<messaging::message_dispatcher>(sessions_)),
      subscription_mgr_(std::move(subscription_mgr)) {
    setup_ssl_context();
}

void server::register_handler(messaging::message_type_range range,
    std::shared_ptr<messaging::message_handler> handler) {
    dispatcher_->register_handler(range, std::move(handler));
}

void server::stop() {
    BOOST_LOG_SEV(lg(), info) << "Stopping server with " << active_connections_ << " active connections...";

    // Stop all active sessions first
    {
        std::lock_guard<std::mutex> lock(sessions_mutex_);
        BOOST_LOG_SEV(lg(), info) << "Stopping " << active_sessions_.size() << " active sessions";
        for (auto& sess : active_sessions_) {
            sess->stop();
        }
        BOOST_LOG_SEV(lg(), info) << "All sessions stopped";
    }

    // Then cancel the accept loop
    BOOST_LOG_SEV(lg(), info) << "Emitting accept loop cancellation signal";
    stop_signal_.emit(boost::asio::cancellation_type::all);
    BOOST_LOG_SEV(lg(), info) << "Accept loop cancellation signal emitted";
}

void server::setup_ssl_context() {
    ssl_ctx_.set_options(
        ssl::context::default_workarounds |
        ssl::context::no_sslv2 |
        ssl::context::no_sslv3 |
        ssl::context::no_tlsv1 |
        ssl::context::no_tlsv1_1 |
        ssl::context::single_dh_use);

    if (options_.certificate_chain_content) {
        ssl_ctx_.use_certificate_chain(
            boost::asio::buffer(*options_.certificate_chain_content));
        BOOST_LOG_SEV(lg(), info) << "SSL context configured with in-memory certificate";
    } else {
        ssl_ctx_.use_certificate_chain_file(options_.certificate_file);
        BOOST_LOG_SEV(lg(), info) << "SSL context configured with certificate file: "
                                  << options_.certificate_file;
    }

    if (options_.private_key_content) {
        ssl_ctx_.use_private_key(
            boost::asio::buffer(*options_.private_key_content), ssl::context::pem);
        BOOST_LOG_SEV(lg(), info) << "SSL context configured with in-memory private key";
    } else {
        ssl_ctx_.use_private_key_file(options_.private_key_file, ssl::context::pem);
        BOOST_LOG_SEV(lg(), info) << "SSL context configured with private key file: "
                                  << options_.private_key_file;
    }
}

boost::asio::awaitable<void> server::run(boost::asio::io_context& io_context,
    std::function<void(std::uint16_t)> on_listening) {
    BOOST_LOG_SEV(lg(), info) << "ORES Server starting on port " << options_.port
                              << ". Identifier: " << options_.server_identifier;
    BOOST_LOG_SEV(lg(), info) << "Messaging version: "
                              << messaging::PROTOCOL_VERSION_MAJOR << "."
                              << messaging::PROTOCOL_VERSION_MINOR;

    if (options_.enable_signal_watching) {
        // Start listening for OS stop signals
        boost::asio::co_spawn(io_context,
            [this, &io_context]() -> boost::asio::awaitable<void> {
                co_await watch_for_stop_signals(io_context);
            },
            boost::asio::detached
    );
    }
    co_await accept_loop(io_context, std::move(on_listening));
}

boost::asio::awaitable<void> server::accept_loop(boost::asio::io_context& io_context,
    std::function<void(std::uint16_t)> on_listening) {
    tcp::acceptor acceptor(
        co_await boost::asio::this_coro::executor,
        tcp::endpoint(tcp::v4(), options_.port));

    auto local_port = acceptor.local_endpoint().port();
    BOOST_LOG_SEV(lg(), info) << "Server listening on port " << local_port;

    if (on_listening) {
        on_listening(local_port);
    }

    while (true) {
        try {
            // Wait if we've reached max connections
            while (active_connections_.load() >= options_.max_connections) {
                BOOST_LOG_SEV(lg(), info) << "Max connections (" << options_.max_connections
                                         << ") reached, waiting.";
                boost::asio::steady_timer timer(co_await boost::asio::this_coro::executor);
                timer.expires_after(std::chrono::milliseconds(100));
                co_await timer.async_wait(
                    boost::asio::bind_cancellation_slot(
                        stop_signal_.slot(),
                        boost::asio::use_awaitable));
            }

            // Accept new connection
            tcp::socket socket = co_await acceptor.async_accept(
                boost::asio::bind_cancellation_slot(
                    stop_signal_.slot(),
                    boost::asio::use_awaitable));
            BOOST_LOG_SEV(lg(), info) << "Accepted connection from "
                                      << socket.remote_endpoint().address().to_string()
                                      << ":" << socket.remote_endpoint().port();

            // Create SSL socket and connection wrapper
            auto conn = std::make_unique<connection>(
                connection::ssl_socket(std::move(socket), ssl_ctx_));

            // Create session with subscription manager
            auto sess = std::make_shared<session>(std::move(conn),
                options_.server_identifier, dispatcher_,
                io_context.get_executor(), subscription_mgr_);

            // Add to active sessions list
            {
                std::lock_guard<std::mutex> lock(sessions_mutex_);
                active_sessions_.push_back(sess);
            }

            // Increment active connections
            ++active_connections_;

            // Spawn session - capture shared_ptr and active_connections reference
            auto self = shared_from_this();
            boost::asio::co_spawn(
                io_context,
                [sess, self]() -> boost::asio::awaitable<void> {
                    co_await sess->run();
                    --self->active_connections_;

                    // Remove from active sessions list
                    {
                        std::lock_guard<std::mutex> lock(self->sessions_mutex_);
                        self->active_sessions_.remove(sess);
                    }

                    BOOST_LOG_SEV(lg(), debug) << "Session completed, active connections: "
                                              << self->active_connections_.load();
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
    BOOST_LOG_SEV(lg(), info) << "Server shut down.";
}

boost::asio::awaitable<void>
server::watch_for_stop_signals(boost::asio::io_context& io_context) {
    boost::asio::signal_set signals(io_context, SIGINT, SIGTERM);
    co_await signals.async_wait(boost::asio::use_awaitable);
    BOOST_LOG_SEV(lg(), info) << "Received stop signal (SIGINT/SIGTERM). Shutting down...";
    stop(); // triggers cancellation via stop_signal_
}

}
