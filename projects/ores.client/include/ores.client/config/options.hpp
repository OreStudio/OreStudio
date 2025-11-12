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
#ifndef ORES_CLIENT_CONFIG_OPTIONS_HPP
#define ORES_CLIENT_CONFIG_OPTIONS_HPP

#include <iosfwd>
#include <optional>
#include <string>
#include <cstdint>
#include "ores.utility/log/logging_options.hpp"

namespace ores::client::config {

/**
 * @brief Options for connecting to the server.
 */
struct connection_options final {
    /**
     * @brief Host to connect to.
     */
    std::string host;

    /**
     * @brief Port to connect to.
     */
    std::uint16_t port;

    /**
     * @brief Client identifier to send in handshake.
     */
    std::string client_identifier;
};

/**
 * @brief Options for logging in to the server.
 */
struct login_options final {
    /**
     * @brief Username to use for login.
     */
    std::string username;

    /**
     * @brief Password to use for login.
     */
    std::string password;
};

/**
 * @brief All of the configuration options required by the client.
 */
struct options final {
    /**
     * @brief Configuration options related to logging, if any.
     */
    std::optional<ores::utility::log::logging_options> logging;

    /**
     * @brief Configuration options for connecting to the server.
     */
    std::optional<connection_options> connection;

    /**
     * @brief Configuration options for logging in to the server.
     */
    std::optional<login_options> login;
};

std::ostream& operator<<(std::ostream& s, const options& v);

}

#endif
