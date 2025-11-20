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
#ifndef ORES_COMMS_NET_SERVER_OPTIONS_HPP
#define ORES_COMMS_NET_SERVER_OPTIONS_HPP

#include <iosfwd>
#include <string>
#include <cstdint>

namespace ores::comms::net {

/**
 * @brief Configuration for the server.
 */
struct server_options final {
    /**
     * @brief TCP port the server listens on.
     */
    std::uint16_t port = 55555;
    /**
     * @brief Maximum number of concurrent client connections.
     */
    std::uint32_t max_connections = 10;
    /**
     * @brief Path to the TLS certificate file (PEM format).
     */
    std::string certificate_file = "server.crt";
    /**
     * @brief Path to the TLS private key file (PEM format).
     */
    std::string private_key_file = "server.key";
    /**
     * @brief Human-readable identifier for the server instance.
     */
    std::string server_identifier = "ores-server";
};

std::ostream& operator<<(std::ostream& s, const server_options& v);

}

#endif
