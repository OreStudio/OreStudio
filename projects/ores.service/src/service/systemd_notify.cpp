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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.service/service/systemd_notify.hpp"
#include <boost/asio/local/datagram_protocol.hpp>

// systemd only exists on Linux, and boost::asio's local (AF_UNIX) sockets
// -- which this uses instead of hand-rolled sockaddr_un/socket()/sendto()
// -- are themselves only available where BOOST_ASIO_HAS_LOCAL_SOCKETS is
// defined (POSIX, not Windows). Nothing to notify on macOS/Windows anyway
// -- notify_systemd_ready() is a no-op there (see the header doc: "safe to
// call unconditionally").
#if defined(BOOST_ASIO_HAS_LOCAL_SOCKETS)

#include <boost/asio/buffer.hpp>
#include <boost/asio/io_context.hpp>
#include <cstdlib>
#include <string>

namespace ores::service {

void notify_systemd_ready() noexcept {
    const char* socket_path = std::getenv("NOTIFY_SOCKET");
    if (!socket_path || socket_path[0] == '\0')
        return;

    try {
        std::string path(socket_path);
        // An abstract socket address (leading '@') is written as a leading
        // NUL byte, not a literal '@' -- systemd's own convention.
        // boost::asio::local::*::endpoint copies path.size() bytes rather
        // than treating it as a NUL-terminated C string, so an embedded
        // NUL here is preserved correctly.
        if (path.front() == '@')
            path.front() = '\0';

        boost::asio::io_context io;
        boost::asio::local::datagram_protocol::socket sock(io);
        sock.open();

        static constexpr char message[] = "READY=1";
        sock.send_to(boost::asio::buffer(message, sizeof(message) - 1),
                     boost::asio::local::datagram_protocol::endpoint(path));
    } catch (...) {
        // Best-effort notification; never throws (see header doc).
    }
}

}

#else

namespace ores::service {

void notify_systemd_ready() noexcept {
}

}

#endif
