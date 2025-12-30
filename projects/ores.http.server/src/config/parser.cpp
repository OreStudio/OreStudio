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
#include "ores.http.server/config/parser.hpp"

#include <ostream>
#include <cstdint>
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include "ores.http.server/config/parser_exception.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.telemetry/log/logging_configuration.hpp"
#include "ores.database/config/database_configuration.hpp"
#include "ores.utility/program_options/environment_mapper_factory.hpp"

namespace {

const std::string more_information("Try '--help' for more information.");
const std::string product_version("OreStudio HTTP Server v" ORES_VERSION);
const std::string build_info(ORES_BUILD_INFO);
const std::string usage_error_msg("Usage error: ");

const std::string help_arg("help");
const std::string version_arg("version");

const std::string server_address_arg("address");
const std::string server_port_arg("port");
const std::string server_max_connections_arg("max-connections");
const std::string server_jwt_secret_arg("jwt-secret");
const std::string server_jwt_issuer_arg("jwt-issuer");
const std::string server_jwt_audience_arg("jwt-audience");
const std::string server_disable_cors_arg("disable-cors");
const std::string server_cors_origins_arg("cors-origins");
const std::string server_identifier_arg("identifier");

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::parsed_options;
using boost::program_options::options_description;

using ores::http::net::http_server_options;
using ores::http_server::config::options;
using ores::http_server::config::parser_exception;

/**
 * @brief Creates the option descriptions.
 */
options_description make_options_description() {
    using ores::database::database_configuration;
    using ores::telemetry::log::logging_configuration;

    options_description god("General");
    god.add_options()
        ("help,h", "Display usage and exit.")
        ("version,v", "Output version information and exit.");

    const auto lod(logging_configuration::make_options_description(
            "ores.http.server.log"));

    options_description sod("HTTP Server");
    sod.add_options()
        ("address,a", value<std::string>()->default_value("0.0.0.0"),
            "Address to bind to. Defaults to '0.0.0.0'.")
        ("port,p", value<std::uint16_t>()->default_value(8080),
            "Port to listen on. Defaults to 8080.")
        ("max-connections,m", value<std::uint32_t>()->default_value(100),
            "Maximum number of concurrent connections. Defaults to 100.")
        ("jwt-secret", value<std::string>()->default_value(""),
            "JWT secret for HS256 authentication.")
        ("jwt-issuer", value<std::string>()->default_value("ores"),
            "JWT issuer for token validation.")
        ("jwt-audience", value<std::string>()->default_value("ores-api"),
            "JWT audience for token validation.")
        ("disable-cors",
            "Disable CORS support. Enabled by default.")
        ("cors-origins", value<std::string>()->default_value("*"),
            "Allowed CORS origins. Defaults to '*'.")
        ("identifier,i", value<std::string>()->default_value("ores-http-server-v1"),
            "Server identifier for responses.");

    const auto dod(database_configuration::make_options_description());

    options_description r;
    r.add(god).add(lod).add(sod).add(dod);
    return r;
}

/**
 * @brief Print help text.
 */
void print_help(const options_description& od, std::ostream& info) {
    info << "ORES HTTP Server is the REST API backend for OreStudio."
         << std::endl
         << "Provides HTTP/REST endpoints with JWT authentication and OpenAPI support."
         << std::endl << std::endl
         << "Usage: ores.http.server [options]"
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
http_server_options read_server_configuration(const variables_map& vm) {
    http_server_options r;

    r.address = vm[server_address_arg].as<std::string>();
    r.port = vm[server_port_arg].as<std::uint16_t>();
    r.max_connections = vm[server_max_connections_arg].as<std::uint32_t>();
    r.jwt_secret = vm[server_jwt_secret_arg].as<std::string>();
    r.jwt_issuer = vm[server_jwt_issuer_arg].as<std::string>();
    r.jwt_audience = vm[server_jwt_audience_arg].as<std::string>();
    r.enable_cors = (vm.count(server_disable_cors_arg) == 0);
    r.cors_allowed_origins = vm[server_cors_origins_arg].as<std::string>();
    r.server_identifier = vm[server_identifier_arg].as<std::string>();

    return r;
}

/**
 * @brief Parses the arguments supplied in the command line and converts them
 * into a configuration object.
 */
std::optional<options>
parse_arguments(const std::vector<std::string>& arguments, std::ostream& info) {
    using ores::database::database_configuration;

    const auto od(make_options_description());
    using ores::utility::program_options::environment_mapper_factory;
    const auto name_mapper(environment_mapper_factory::make_mapper("HTTP_SERVER"));

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
    using ores::telemetry::log::logging_configuration;

    options r;
    r.logging = logging_configuration::read_options(vm);
    r.server = read_server_configuration(vm);
    r.database = database_configuration::read_options(vm);

    return r;
}

}

namespace ores::http_server::config {

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
