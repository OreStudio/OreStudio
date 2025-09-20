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
#include <boost/cobalt.hpp>
#include <boost/cobalt/main.hpp>

#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/signal_set.hpp>
#include <boost/asio/write.hpp>
#include <array>

namespace cobalt = boost::cobalt;
using boost::asio::ip::tcp;
using tcp_acceptor = cobalt::use_op_t::as_default_on_t<tcp::acceptor>;
using tcp_socket   = cobalt::use_op_t::as_default_on_t<tcp::socket>;

namespace {

cobalt::promise<void> echo(tcp_socket socket) {
    try {
        std::array<char, 4096> data;
        while (socket.is_open()) {
            std::size_t n = co_await socket.async_read_some(boost::asio::buffer(data)); // <3>
            co_await async_write(socket, boost::asio::buffer(data, n));
        }
    } catch (std::exception& e) {
        std::printf("echo: exception: %s\n", e.what());
    }
}

cobalt::generator<tcp_socket> listen() {
    tcp_acceptor acceptor({co_await cobalt::this_coro::executor}, {tcp::v4(), 55555});
    for (;;) {
        tcp_socket sock = co_await acceptor.async_accept();
        co_yield std::move(sock);
    }
    co_return tcp_socket{acceptor.get_executor()};
}

cobalt::promise<void> run_server(cobalt::wait_group & workers) {
    auto l = listen();
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
    co_await cobalt::with(cobalt::wait_group(), &run_server);
    co_return 0u;
}
