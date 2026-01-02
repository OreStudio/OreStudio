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
#include "ores.comms/config/server_configuration.hpp"

namespace ores::comms::config {

namespace {

const std::string port_arg("port");
const std::string max_connections_arg("max-connections");
const std::string identifier_arg("identifier");
const std::string certificate_arg("certificate");
const std::string private_key_arg("private-key");

}

boost::program_options::options_description
server_configuration::make_options_description(
    std::uint16_t default_port,
    std::uint32_t default_max_connections,
    const std::string& default_identifier,
    bool include_ssl) {

    using boost::program_options::value;
    using boost::program_options::options_description;

    options_description r("Server");
    r.add_options()
        ("port,p", value<std::uint16_t>()->default_value(default_port),
            "Port to listen on.")
        ("max-connections,m", value<std::uint32_t>()->default_value(default_max_connections),
            "Maximum number of concurrent connections.")
        ("identifier,i", value<std::string>()->default_value(default_identifier),
            "Server identifier for handshake.");

    if (include_ssl) {
        r.add_options()
            ("certificate,c", value<std::string>()->default_value("server.crt"),
                "Path to SSL certificate file.")
            ("private-key,k", value<std::string>()->default_value("server.key"),
                "Path to SSL private key file.");
    }

    return r;
}

net::server_options server_configuration::
read_options(const boost::program_options::variables_map& vm) {
    net::server_options r;

    r.port = vm[port_arg].as<std::uint16_t>();
    r.max_connections = vm[max_connections_arg].as<std::uint32_t>();
    r.server_identifier = vm[identifier_arg].as<std::string>();

    if (vm.count(certificate_arg) != 0) {
        r.certificate_file = vm[certificate_arg].as<std::string>();
    }

    if (vm.count(private_key_arg) != 0) {
        r.private_key_file = vm[private_key_arg].as<std::string>();
    }

    return r;
}

}
