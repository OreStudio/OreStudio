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

#include <cstdlib>
#include <cstring>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

namespace ores::service {

void notify_systemd_ready() noexcept {
    const char* socket_path = std::getenv("NOTIFY_SOCKET");
    if (!socket_path || socket_path[0] == '\0') return;

    sockaddr_un addr{};
    addr.sun_family = AF_UNIX;
    std::size_t path_len = std::strlen(socket_path);
    if (path_len >= sizeof(addr.sun_path)) return;

    // An abstract socket address (leading '@') is written as a leading NUL
    // byte, not a literal '@' -- systemd's own convention.
    std::size_t offset = 0;
    if (socket_path[0] == '@') {
        addr.sun_path[0] = '\0';
        offset = 1;
    }
    std::memcpy(addr.sun_path + offset, socket_path + offset, path_len - offset);
    socklen_t addr_len = static_cast<socklen_t>(
        offsetof(sockaddr_un, sun_path) + path_len);

    int fd = ::socket(AF_UNIX, SOCK_DGRAM, 0);
    if (fd < 0) return;

    static constexpr char message[] = "READY=1";
    ::sendto(fd, message, sizeof(message) - 1, 0,
        reinterpret_cast<const sockaddr*>(&addr), addr_len);
    ::close(fd);
}

}
