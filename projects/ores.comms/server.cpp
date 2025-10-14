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
#include <iostream>

namespace ores::comms {

server::server(server_config config)
    : config_(std::move(config)),
      ssl_ctx_(ssl::context::tlsv13) {
    setup_ssl_context();
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

    std::printf("SSL context configured with certificate: %s\n",
               config_.certificate_file.c_str());
}

cobalt::promise<void> server::run() {
    std::printf("ORES Server starting on port %d (identifier: %s)\n",
               config_.port, config_.server_identifier.c_str());
    std::printf("Protocol version: %d.%d\n",
               protocol::PROTOCOL_VERSION_MAJOR,
               protocol::PROTOCOL_VERSION_MINOR);

    co_await cobalt::with(cobalt::wait_group(), [this](auto& wg) {
        return accept_loop(wg);
    });
}

cobalt::promise<void> server::accept_loop(cobalt::wait_group& workers) {
    using tcp_acceptor = cobalt::use_op_t::as_default_on_t<tcp::acceptor>;

    tcp_acceptor acceptor(
        {co_await cobalt::this_coro::executor},
        {tcp::v4(), config_.port});

    std::printf("Server listening on port %d\n", config_.port);

    while (true) {
        try {
            // Wait if we've reached max connections
            if (workers.size() >= config_.max_connections) {
                std::printf("Max connections (%d) reached, waiting...\n",
                           config_.max_connections);
                co_await workers.wait_one();
            }

            // Accept new connection
            tcp::socket socket = co_await acceptor.async_accept();
            std::printf("Accepted connection from %s:%d\n",
                       socket.remote_endpoint().address().to_string().c_str(),
                       socket.remote_endpoint().port());

            // Create SSL socket
            connection::ssl_socket ssl_sock(std::move(socket), ssl_ctx_);

            // Create connection wrapper
            auto conn = std::make_unique<connection>(std::move(ssl_sock));

            // Create and start session
            auto sess = std::make_unique<session>(std::move(conn), config_.server_identifier);

            // Add session to worker group
            workers.push_back(sess->run());

        } catch (const std::exception& e) {
            std::printf("Accept loop error: %s\n", e.what());
        }
    }
}

}
