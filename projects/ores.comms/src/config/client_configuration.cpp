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
#include "ores.comms/config/client_configuration.hpp"

#include <stdexcept>

namespace ores::comms::config {

namespace {

const std::string connect_host_arg("connect-host");
const std::string connect_port_arg("connect-port");
const std::string connect_identifier_arg("connect-identifier");
const std::string login_username_arg("login-username");
const std::string login_password_arg("login-password");

}

boost::program_options::options_description
client_configuration::make_options_description(
    std::uint16_t default_port,
    const std::string& default_identifier,
    bool include_auth) {

    using boost::program_options::value;
    using boost::program_options::options_description;

    options_description r("Connection");
    r.add_options()
        ("connect-host", value<std::string>()->default_value("localhost"),
            "Host to connect to.")
        ("connect-port", value<std::uint16_t>()->default_value(default_port),
            "Port to connect to.")
        ("connect-identifier", value<std::string>()->default_value(default_identifier),
            "Client identifier for handshake.");

    if (include_auth) {
        options_description auth("Authentication");
        auth.add_options()
            ("login-username", value<std::string>(),
                "Username for authentication.")
            ("login-password", value<std::string>(),
                "Password for authentication.");
        r.add(auth);
    }

    return r;
}

std::optional<net::client_options> client_configuration::
read_options(const boost::program_options::variables_map& vm) {
    const bool has_host = vm.count(connect_host_arg) != 0;
    const bool has_port = vm.count(connect_port_arg) != 0;
    const bool has_identifier = vm.count(connect_identifier_arg) != 0;

    if (!has_host && !has_port && !has_identifier) {
        return {};
    }

    net::client_options r;

    if (has_host) {
        r.host = vm[connect_host_arg].as<std::string>();
    }

    if (has_port) {
        r.port = vm[connect_port_arg].as<std::uint16_t>();
    }

    if (has_identifier) {
        r.client_identifier = vm[connect_identifier_arg].as<std::string>();
    }

    return r;
}

std::optional<login_options> client_configuration::
read_login_options(const boost::program_options::variables_map& vm) {
    const bool has_username = vm.count(login_username_arg) != 0;
    const bool has_password = vm.count(login_password_arg) != 0;

    if (!has_username && !has_password) {
        return {};
    }

    if (has_username != has_password) {
        if (!has_username) {
            throw std::runtime_error(
                "Login password provided but username is missing.");
        } else {
            throw std::runtime_error(
                "Login username provided but password is missing.");
        }
    }

    login_options r;
    r.username = vm[login_username_arg].as<std::string>();
    r.password = vm[login_password_arg].as<std::string>();

    return r;
}

}
