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
#include <format>
#include <ostream>
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include "ores.service/config/parser_exception.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.utility/log/severity_level.hpp"
#include "ores.service/config/parser.hpp"

namespace {

const std::string more_information("Try --help' for more information.");
const std::string product_version("OreStudio Service v" ORES_VERSION);
const std::string build_info(ORES_BUILD_INFO);
const std::string usage_error_msg("Usage error: ");

const std::string help_arg("help");
const std::string version_arg("version");

const std::string logging_log_enabled_arg("log-enabled");
const std::string logging_log_to_console_arg("log-to-console");
const std::string logging_log_level_arg("log-level");
const std::string logging_log_dir_arg("log-directory");
const std::string logging_log_filename_arg("log-filename");
const std::string logging_log_level_info("info");

const std::string server_port_arg("port");
const std::string server_max_connections_arg("max-connections");
const std::string server_certificate_arg("certificate");
const std::string server_private_key_arg("private-key");
const std::string server_identifier_arg("identifier");

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::parsed_options;
using boost::program_options::options_description;

using ores::utility::log::logging_options;
using ores::service::config::options;
using ores::service::config::server_options;
using ores::service::config::parser_exception;

/**
 * @brief Creates the option descriptions.
 */
options_description make_options_description() {
    options_description god("General");
    god.add_options()
        ("help,h", "Display usage and exit.")
        ("version,v", "Output version information and exit.");

    options_description lod("Logging");
    lod.add_options()
        ("log-enabled,e", "Generate a log file.")
        ("log-level,l", value<std::string>(),
            "What level to use for logging. Valid values: trace, debug, info, "
            "warn, error. Defaults to info.")
        ("log-to-console",
            "Output logging to the console, as well as to file.")
        ("log-directory", value<std::string>(),
            "Where to place the log files. Defaults to 'log'.")
        ("log-filename", value<std::string>(),
            "Name of the log file. Defaults to 'ores.service.log'.");

    options_description sod("Server");
    sod.add_options()
        ("port,p", value<uint16_t>(),
            "Port to listen on. Defaults to 55555.")
        ("max-connections,m", value<uint32_t>(),
            "Maximum number of concurrent connections. Defaults to 10.")
        ("certificate,c", value<std::string>(),
            "Path to SSL certificate file. Defaults to 'server.crt'.")
        ("private-key,k", value<std::string>(),
            "Path to SSL private key file. Defaults to 'server.key'.")
        ("identifier,i", value<std::string>(),
            "Server identifier for handshake. Defaults to 'ores-service-v1'.");

    options_description r;
    r.add(god).add(lod).add(sod);
    return r;
}

/**
 * @brief Print help text.
 */
void print_help(const options_description& od, std::ostream& info) {
    info << "ORES Service is the backend server for OreStudio."
         << std::endl
         << "It provides network communication capabilities for the Open Source Risk Engine (ORE)."
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
 * @brief Reads the logging configuration from the variables map.
 */
std::optional<logging_options>
read_logging_configuration(const variables_map& vm) {
    const auto enabled(vm.count(logging_log_enabled_arg) != 0);
    if (!enabled)
        return {};

    logging_options r;

    // Set log filename
    if (vm.count(logging_log_filename_arg) != 0) {
        r.filename = vm[logging_log_filename_arg].as<std::string>();
    } else {
        r.filename = "ores.service.log";
    }

    r.output_to_console = vm.count(logging_log_to_console_arg) != 0;

    // Set log directory
    if (vm.count(logging_log_dir_arg) != 0) {
        r.output_directory = vm[logging_log_dir_arg].as<std::string>();
    } else {
        r.output_directory = "log";
    }

    // Set log level
    if (vm.count(logging_log_level_arg) != 0) {
        const auto s(vm[logging_log_level_arg].as<std::string>());
        try {
            using ores::utility::log::to_severity_level;
            to_severity_level(s);
            r.severity = s;
        } catch(const std::exception&) {
            BOOST_THROW_EXCEPTION(parser_exception(
                    std::format("Log level is invalid: {}!", s)));
        }
    } else {
        r.severity = logging_log_level_info;
    }

    return r;
}

/**
 * @brief Reads the server configuration from the variables map.
 */
server_options read_server_configuration(const variables_map& vm) {
    server_options r;

    if (vm.count(server_port_arg) != 0)
        r.port = vm[server_port_arg].as<uint16_t>();

    if (vm.count(server_max_connections_arg) != 0)
        r.max_connections = vm[server_max_connections_arg].as<uint32_t>();

    if (vm.count(server_certificate_arg) != 0)
        r.certificate_file = vm[server_certificate_arg].as<std::string>();

    if (vm.count(server_private_key_arg) != 0)
        r.private_key_file = vm[server_private_key_arg].as<std::string>();

    if (vm.count(server_identifier_arg) != 0)
        r.server_identifier = vm[server_identifier_arg].as<std::string>();

    return r;
}

/**
 * @brief Parses the arguments supplied in the command line and converts them
 * into a configuration object.
 */
std::optional<options>
parse_arguments(const std::vector<std::string>& arguments, std::ostream& info) {
    const auto od(make_options_description());

    variables_map vm;
    boost::program_options::store(
        boost::program_options::command_line_parser(arguments).
        options(od).run(), vm);

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
    options r;
    r.logging = read_logging_configuration(vm);
    r.server = read_server_configuration(vm);
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
