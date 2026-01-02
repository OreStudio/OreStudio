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
#ifndef ORES_COMMS_CONFIG_CLIENT_CONFIGURATION_HPP
#define ORES_COMMS_CONFIG_CLIENT_CONFIGURATION_HPP

#include <string>
#include <cstdint>
#include <optional>
#include <boost/program_options.hpp>
#include "ores.comms/net/client_options.hpp"

namespace ores::comms::config {

/**
 * @brief Login credentials for authentication.
 */
struct login_options {
    std::string username;
    std::string password;
};

/**
 * @brief Provides command-line options for client configuration.
 *
 * Standard client options:
 *       --connect-host HOST       Host to connect to
 *       --connect-port PORT       Port to connect to
 *       --connect-identifier ID   Client identifier for handshake
 *
 * Authentication options (when include_auth is true):
 *       --login-username USER     Username for authentication
 *       --login-password PASS     Password for authentication
 */
class client_configuration final {
public:
    client_configuration() = delete;

    /**
     * @brief Creates the options description for client CLI arguments.
     *
     * @param default_port Default port number.
     * @param default_identifier Default client identifier.
     * @param include_auth Whether to include authentication options.
     * @return options_description for client configuration.
     */
    static boost::program_options::options_description
    make_options_description(
        std::uint16_t default_port,
        const std::string& default_identifier,
        bool include_auth = false);

    /**
     * @brief Reads client options from parsed variables map.
     *
     * Always returns a client_options object, populated with either
     * user-provided values or the defaults.
     *
     * @param vm Parsed command-line options.
     * @return client_options populated from the variables map.
     */
    static net::client_options
    read_options(const boost::program_options::variables_map& vm);

    /**
     * @brief Reads login options from parsed variables map.
     *
     * Returns std::nullopt if no login options are provided.
     * Throws if only username or only password is provided.
     *
     * @param vm Parsed command-line options.
     * @return login_options if both username and password were provided.
     * @throws std::runtime_error if only one of username/password is provided.
     */
    static std::optional<login_options>
    read_login_options(const boost::program_options::variables_map& vm);
};

}

#endif
