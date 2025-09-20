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
#include <list>

// tag::decls[]
namespace cobalt = boost::cobalt;
using boost::asio::ip::tcp;
using boost::asio::detached;
using tcp_acceptor = cobalt::use_op_t::as_default_on_t<tcp::acceptor>;
using tcp_socket   = cobalt::use_op_t::as_default_on_t<tcp::socket>;
namespace this_coro = boost::cobalt::this_coro;
//end::decls[]

// tag::echo[]
cobalt::promise<void> echo(tcp_socket socket)
{
  try // <1>
  {
    char data[4096];
    while (socket.is_open()) // <2>
    {
      std::size_t n = co_await socket.async_read_some(boost::asio::buffer(data)); // <3>
      co_await async_write(socket, boost::asio::buffer(data, n)); // <4>
    }
  }
  catch (std::exception& e)
  {
    std::printf("echo: exception: %s\n", e.what());
  }
}
// end::echo[]


// tag::listen[]
cobalt::generator<tcp_socket> listen()
{
  tcp_acceptor acceptor({co_await cobalt::this_coro::executor}, {tcp::v4(), 55555});
  for (;;) // <1>
  {
    tcp_socket sock = co_await acceptor.async_accept(); // <2>
    co_yield std::move(sock); // <3>
  }
  co_return tcp_socket{acceptor.get_executor()}; // <4>
}
// end::listen[]

// tag::run_server[]
cobalt::promise<void> run_server(cobalt::wait_group & workers)
{
  auto l = listen(); // <1>
  while (true)
  {
    if (workers.size() == 10u)
      co_await workers.wait_one();  // <2>
    else
      workers.push_back(echo(co_await l)); // <3>
  }
}
// end::run_server[]

// tag::main[]
cobalt::main co_main(int argc, char ** argv)
{
  co_await cobalt::with(cobalt::wait_group(), &run_server); // <1>
  co_return 0u;
}
// end::main[]
