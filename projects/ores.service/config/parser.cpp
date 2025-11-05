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
#include <ostream>
#include <cstdint>
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include "ores.service/config/parser_exception.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.utility/log/logging_configuration.hpp"
#include "ores.utility/database/database_configuration.hpp"
#include "ores.service/config/parser.hpp"

namespace {

const std::string more_information("Try --help' for more information.");
const std::string product_version("OreStudio Service v" ORES_VERSION);
const std::string build_info(ORES_BUILD_INFO);
const std::string usage_error_msg("Usage error: ");

const std::string help_arg("help");
const std::string version_arg("version");

const std::string server_port_arg("port");
const std::string server_max_connections_arg("max-connections");
const std::string server_certificate_arg("certificate");
const std::string server_private_key_arg("private-key");
const std::string server_identifier_arg("identifier");

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::parsed_options;
using boost::program_options::options_description;

using ores::service::config::options;
using ores::service::config::server_options;
using ores::service::config::parser_exception;

/**
 * @brief Creates the option descriptions.
 */
options_description make_options_description() {
    using ores::utility::database::database_configuration;
    using ores::utility::log::logging_configuration;

    options_description god("General");
    god.add_options()
        ("help,h", "Display usage and exit.")
        ("version,v", "Output version information and exit.");

    const auto lod(logging_configuration::make_options_description(
            "ores.service.log"));

    options_description sod("Server");
    sod.add_options()
        ("port,p", value<std::uint16_t>()->default_value(55555),
            "Port to listen on.")
        ("max-connections,m", value<std::uint32_t>()->default_value(10),
            "Maximum number of concurrent connections. Defaults to 10.")
        ("certificate,c", value<std::string>()->default_value("server.crt"),
            "Path to SSL certificate file.")
        ("private-key,k", value<std::string>()->default_value("server.key"),
            "Path to SSL private key file. Defaults to 'server.key'.")
        ("identifier,i", value<std::string>()->default_value("ores-service-v1"),
            "Server identifier for handshake.");

    const auto dod(database_configuration::make_options_description());

    options_description r;
    r.add(god).add(lod).add(sod).add(dod);
    return r;
}

/**
 * @brief Print help text.
 */
void print_help(const options_description& od, std::ostream& info) {
    info << "ORES Service is the backend server for OreStudio."
         << std::endl
         << "Provides network communication capabilities for OreStudio)."
         << std::endl << std::endl
         << "Usage: ores.service [options]"
         << std::endl << std::endl
         << od << std::endl;
}

/**
 * @brief Print the program's version details.
 */
void version(std::ostream& info) {
    info << product_version << std::endl
         << "Copyright (C) 2025 Marco Craveiro." << std::endl
         << "License GPLv3: GNU GPL version 3 or later "
         << "<http://gnu.org/licenses/gpl.html>." << std::endl
         << "This is free software: you are free to change and redistribute it."
         << std::endl << "There is NO WARRANTY, to the extent permitted by law."
         << std::endl;

    if (!build_info.empty()) {
        info << build_info << std::endl;
        info << "IMPORTANT: build details are NOT for security purposes."
             << std::endl;
    }
}

/**
 * @brief Reads the server configuration from the variables map.
 */
server_options read_server_configuration(const variables_map& vm) {
    server_options r;

    r.port = vm[server_port_arg].as<std::uint16_t>();
    r.max_connections = vm[server_max_connections_arg].as<std::uint32_t>();
    r.certificate_file = vm[server_certificate_arg].as<std::string>();
    r.private_key_file = vm[server_private_key_arg].as<std::string>();
    r.server_identifier = vm[server_identifier_arg].as<std::string>();

    return r;
}


/**
 * @brief Parses the arguments supplied in the command line and converts them
 * into a configuration object.
 */
std::optional<options>
parse_arguments(const std::vector<std::string>& arguments, std::ostream& info) {
    using ores::utility::database::database_configuration;

    const auto od(make_options_description());
    const auto name_mapper(database_configuration::make_environment_mapper());

    variables_map vm;
    boost::program_options::store(
        boost::program_options::command_line_parser(arguments).
        options(od).run(), vm);
    boost::program_options::store(
        boost::program_options::parse_environment(od, name_mapper), vm);

    const bool has_version(vm.count(version_arg) != 0);
    const bool has_help(vm.count(help_arg) != 0);

    // Handle help and version
    if (has_help) {
        print_help(od, info);
        return {};
    }

    if (has_version) {
        version(info);
        return {};
    }

    // Parse configuration
    using ores::utility::log::logging_configuration;

    options r;
    r.logging = logging_configuration::read_options(vm);
    r.server = read_server_configuration(vm);
    r.database = database_configuration::read_options(vm);
    return r;
}

}

namespace ores::service::config {

std::optional<options>
parser::parse(const std::vector<std::string>& arguments,
    std::ostream& info, std::ostream& err) const {

    try {
        return parse_arguments(arguments, info);
    } catch(const parser_exception& e) {
        err << usage_error_msg << e.what() << std::endl
            << more_information << std::endl;
        BOOST_THROW_EXCEPTION(e);
    } catch (const boost::program_options::error& e) {
        err << usage_error_msg << e.what() << std::endl
            << more_information << std::endl;
        BOOST_THROW_EXCEPTION(parser_exception(e.what()));
    }
}

}
