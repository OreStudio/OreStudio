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
#include <boost/cobalt.hpp>
#include <boost/cobalt/spawn.hpp>
#include <boost/asio.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <iostream>
#include <string>

namespace cobalt = boost::cobalt;
namespace asio = boost::asio;
using asio::ip::tcp;

namespace {

cobalt::task<void> echo_client(cobalt::executor exec, const std::string& host, const std::string& port)
{
    try
    {
        tcp::socket socket(exec);
        tcp::resolver resolver(exec);

        auto endpoints = co_await resolver.async_resolve(host, port, cobalt::use_op);
        co_await asio::async_connect(socket, endpoints, cobalt::use_op);

        std::string message = "Hello, Echo Server!";
        std::vector<char> buffer(1024);

        co_await socket.async_send(asio::buffer(message), cobalt::use_op);
        std::size_t bytes_received = co_await socket.async_receive(asio::buffer(buffer), cobalt::use_op);

        std::cout << "Echoed response: " << std::string(buffer.data(), bytes_received) << std::endl;
    }
    catch (const std::exception& e)
    {
        std::cerr << "Exception: " << e.what() << std::endl;
    }
}

}

int main(int /*argc*/, char */*argv*/[])
{
 asio::io_context ctx;
    cobalt::spawn(ctx, echo_client(ctx.get_executor(), "127.0.0.1", "55555"), asio::detached);
    ctx.run();
    return 0;
}
