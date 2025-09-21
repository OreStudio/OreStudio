/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <array>
#include <boost/cobalt.hpp>
#include <boost/cobalt/main.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/signal_set.hpp>
#include <boost/asio/write.hpp>
#include <boost/asio/ssl.hpp>

namespace cobalt = boost::cobalt;
using boost::asio::ip::tcp;
namespace ssl = boost::asio::ssl;
using tcp_acceptor = cobalt::use_op_t::as_default_on_t<tcp::acceptor>;
using ssl_socket   = ssl::stream<tcp::socket>;

namespace {

cobalt::promise<void> echo(ssl_socket socket) {
    try {
        co_await socket.async_handshake(ssl::stream_base::server, cobalt::use_op);
        std::array<char, 4096> data;
        while (socket.lowest_layer().is_open()) {
            std::size_t n = co_await socket.async_read_some(boost::asio::buffer(data), cobalt::use_op);
            co_await async_write(socket, boost::asio::buffer(data, n), cobalt::use_op);
        }
    } catch (std::exception& e) {
        std::printf("echo: exception: %s\n", e.what());
    }
}

cobalt::generator<ssl_socket> listen(ssl::context& ctx) {
    tcp_acceptor acceptor({co_await cobalt::this_coro::executor}, {tcp::v4(), 55555});
    for (;;) {
        tcp::socket sock = co_await acceptor.async_accept();
        ssl_socket ssl_sock(std::move(sock), ctx);
        co_yield std::move(ssl_sock);
    }
    co_return ssl_socket{tcp::socket{acceptor.get_executor()}, ctx};
}

cobalt::promise<void> run_server(cobalt::wait_group & workers, ssl::context& ctx) {
    auto l = listen(ctx);
    while (true)
    {
        if (workers.size() == 10u)
            co_await workers.wait_one();
        else
            workers.push_back(echo(co_await l));
    }
}

}

cobalt::main co_main(int argc, char ** argv) {
    ssl::context ctx(ssl::context::tlsv12);
    ctx.set_options(ssl::context::default_workarounds
                  | ssl::context::no_sslv2
                  | ssl::context::single_dh_use);
    ctx.use_certificate_chain_file("server.crt");
    ctx.use_private_key_file("server.key", ssl::context::pem);

    co_await cobalt::with(cobalt::wait_group(), [&ctx](auto& wg) {
        return run_server(wg, ctx);
    });
    co_return 0u;
}
