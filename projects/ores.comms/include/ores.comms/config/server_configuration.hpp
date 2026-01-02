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
#ifndef ORES_COMMS_CONFIG_SERVER_CONFIGURATION_HPP
#define ORES_COMMS_CONFIG_SERVER_CONFIGURATION_HPP

#include <string>
#include <cstdint>
#include <boost/program_options.hpp>
#include "ores.comms/net/server_options.hpp"

namespace ores::comms::config {

/**
 * @brief Provides command-line options for server configuration.
 *
 * Standard server options:
 *   -p, --port PORT           Port to listen on
 *   -m, --max-connections N   Maximum concurrent connections
 *   -i, --identifier ID       Server identifier for handshake
 *
 * SSL options (when include_ssl is true):
 *   -c, --certificate FILE    Path to SSL certificate file
 *   -k, --private-key FILE    Path to SSL private key file
 */
class server_configuration final {
public:
    server_configuration() = delete;

    /**
     * @brief Creates the options description for server CLI arguments.
     *
     * @param default_port Default port number.
     * @param default_max_connections Default max connections.
     * @param default_identifier Default server identifier.
     * @param include_ssl Whether to include SSL certificate options.
     * @return options_description for server configuration.
     */
    static boost::program_options::options_description
    make_options_description(
        std::uint16_t default_port,
        std::uint32_t default_max_connections,
        const std::string& default_identifier,
        bool include_ssl = false);

    /**
     * @brief Reads server options from parsed variables map.
     *
     * @param vm Parsed command-line options.
     * @return server_options populated from the variables map.
     */
    static net::server_options
    read_options(const boost::program_options::variables_map& vm);
};

}

#endif
