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
#include "ores.comms/server.hpp"
#include "ores.comms/session.hpp"
#include "ores.utility/log/logger.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.comms.server"));

}

namespace ores::comms {

server::server(server_config config)
    : config_(std::move(config)),
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

    ssl_ctx_.use_certificate_chain_file(config_.certificate_file);
    ssl_ctx_.use_private_key_file(config_.private_key_file, ssl::context::pem);

    BOOST_LOG_SEV(lg, info) << "SSL context configured with certificate: "
                             << config_.certificate_file;
}

cobalt::promise<void> server::run() {
    BOOST_LOG_SEV(lg, info) << "ORES Server starting on port " << config_.port
                             << " (identifier: " << config_.server_identifier << ")";
    BOOST_LOG_SEV(lg, info) << "Protocol version: "
                             << protocol::PROTOCOL_VERSION_MAJOR << "."
                             << protocol::PROTOCOL_VERSION_MINOR;

    co_await cobalt::with(cobalt::wait_group(), [this](auto& wg) {
        return accept_loop(wg);
    });
}

cobalt::promise<void> server::accept_loop(cobalt::wait_group& workers) {
    using tcp_acceptor = cobalt::use_op_t::as_default_on_t<tcp::acceptor>;

    tcp_acceptor acceptor(
        {co_await cobalt::this_coro::executor},
        {tcp::v4(), config_.port});

    BOOST_LOG_SEV(lg, info) << "Server listening on port " << config_.port;

    while (true) {
        try {
            // Wait if we've reached max connections
            if (workers.size() >= config_.max_connections) {
                BOOST_LOG_SEV(lg, info) << "Max connections (" << config_.max_connections
                                         << ") reached, waiting...";
                co_await workers.wait_one();
            }

            // Accept new connection
            tcp::socket socket = co_await acceptor.async_accept();
            BOOST_LOG_SEV(lg, info) << "Accepted connection from "
                                     << socket.remote_endpoint().address().to_string()
                                     << ":" << socket.remote_endpoint().port();

            // Create SSL socket and connection wrapper
            auto conn = std::make_unique<connection>(
                connection::ssl_socket(std::move(socket), ssl_ctx_));

            // Create session and spawn it
            auto sess = std::make_shared<session>(std::move(conn), config_.server_identifier, dispatcher_);

            // Add session to worker group - capture sess by value to keep it alive
            workers.push_back([](std::shared_ptr<session> s) -> cobalt::promise<void> {
                co_await s->run();
            }(sess));

        } catch (const boost::system::system_error& e) {
            // Check if operation was cancelled (shutdown signal)
            if (e.code() == boost::asio::error::operation_aborted) {
                BOOST_LOG_SEV(lg, info) << "Server shutting down...";
                break;
            }
            BOOST_LOG_SEV(lg, error) << "Accept loop error: " << e.what();
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg, error) << "Accept loop error: " << e.what();
        }
    }
}

}
