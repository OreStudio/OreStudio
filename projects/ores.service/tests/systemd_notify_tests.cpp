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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.platform/environment/environment.hpp"
#include "ores.service/service/systemd_notify.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

// The notification travels over an AF_UNIX datagram socket, which boost::asio
// exposes only where BOOST_ASIO_HAS_LOCAL_SOCKETS is defined. On the platforms
// that lack it, notify_systemd_ready() is the documented no-op overload.
#if defined(BOOST_ASIO_HAS_LOCAL_SOCKETS)

#    include <boost/asio/buffer.hpp>
#    include <boost/asio/error.hpp>
#    include <boost/asio/io_context.hpp>
#    include <boost/asio/local/datagram_protocol.hpp>
#    include <cstdio>
#    include <unistd.h>

namespace {

const std::string tags("[systemd_notify]");
const std::string ready_message("READY=1");

using datagram_socket = boost::asio::local::datagram_protocol::socket;
using datagram_endpoint = boost::asio::local::datagram_protocol::endpoint;
using ores::platform::environment::environment;

// Binds a receiver before the notification is sent, so the datagram is already
// queued when the blocking receive runs.
datagram_socket bind_receiver(boost::asio::io_context& io, const std::string& name) {
    datagram_socket receiver(io);
    receiver.open();
    receiver.bind(datagram_endpoint(name));
    return receiver;
}

}

// The socket name carries the process id so a leftover socket from an
// interrupted run cannot make the bind fail.
TEST_CASE("notify_systemd_ready sends READY=1 to the socket in NOTIFY_SOCKET", tags) {
    const std::string path("/tmp/ores.service.notify." + std::to_string(::getpid()) + ".sock");
    std::remove(path.c_str());

    boost::asio::io_context io;
    auto receiver = bind_receiver(io, path);
    environment::set_value("NOTIFY_SOCKET", path);

    ores::service::service::notify_systemd_ready();

    char buffer[16] = {};
    const auto received = receiver.receive(boost::asio::buffer(buffer));

    environment::unset_value("NOTIFY_SOCKET");
    std::remove(path.c_str());

    REQUIRE(received == ready_message.size());
    REQUIRE(std::string(buffer, received) == ready_message);
}

// systemd names an abstract socket with a leading '@'; on the wire that is a
// leading NUL byte, and the notification must not go to a literal file named
// "@...".
TEST_CASE("notify_systemd_ready sends to an abstract socket named with a leading at", tags) {
    const std::string abstract_name(1, '\0');
    const std::string socket_name("ores.service.notify." + std::to_string(::getpid()));
    const std::string path = abstract_name + socket_name;

    boost::asio::io_context io;
    auto receiver = bind_receiver(io, path);
    environment::set_value("NOTIFY_SOCKET", "@" + socket_name);

    ores::service::service::notify_systemd_ready();

    char buffer[16] = {};
    const auto received = receiver.receive(boost::asio::buffer(buffer));

    environment::unset_value("NOTIFY_SOCKET");

    REQUIRE(received == ready_message.size());
    REQUIRE(std::string(buffer, received) == ready_message);
}

TEST_CASE("notify_systemd_ready stops sending once NOTIFY_SOCKET is unset", tags) {
    const std::string path("/tmp/ores.service.notify.none." + std::to_string(::getpid()) + ".sock");
    std::remove(path.c_str());

    boost::asio::io_context io;
    auto receiver = bind_receiver(io, path);

    // First prove the receiver reads a notification, so the silence below is
    // the unset variable and not a mis-bound socket.
    environment::set_value("NOTIFY_SOCKET", path);
    ores::service::service::notify_systemd_ready();

    char delivered[16] = {};
    REQUIRE(receiver.receive(boost::asio::buffer(delivered)) == ready_message.size());

    environment::unset_value("NOTIFY_SOCKET");
    ores::service::service::notify_systemd_ready();

    receiver.non_blocking(true);
    char silent[16] = {};
    boost::system::error_code ec;
    receiver.receive(boost::asio::buffer(silent), 0, ec);

    std::remove(path.c_str());

    // No second notification was sent, so this receive would have blocked.
    REQUIRE(ec == boost::asio::error::would_block);
}

#endif
