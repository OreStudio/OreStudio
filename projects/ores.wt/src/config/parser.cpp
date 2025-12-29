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
#include "ores.wt/config/parser.hpp"
#include "ores.wt/config/parser_exception.hpp"

#include <ostream>
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include "ores.utility/version/version.hpp"
#include "ores.telemetry/log/logging_configuration.hpp"
#include "ores.database/config/database_configuration.hpp"
#include "ores.utility/program_options/environment_mapper_factory.hpp"

namespace {

const std::string more_information("Try --help' for more information.");
const std::string product_version("OreStudio Web v" ORES_VERSION);
const std::string build_info(ORES_BUILD_INFO);
const std::string usage_error_msg("Usage error: ");

const std::string help_arg("help");
const std::string version_arg("version");

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::options_description;

using ores::wt::config::options;
using ores::wt::config::parser_exception;

options_description make_options_description() {
    using ores::database::database_configuration;
    using ores::telemetry::log::logging_configuration;

    options_description god("General");
    god.add_options()
        ("help,h", "Display usage and exit.")
        ("version,v", "Output version information and exit.");

    const auto lod(logging_configuration::make_options_description("ores.wt.log"));
    const auto dod(database_configuration::make_options_description());

    options_description r;
    r.add(god).add(lod).add(dod);
    return r;
}

void print_help(const options_description& od, std::ostream& info) {
    info << "OreStudio Web is the web frontend for OreStudio."
         << std::endl
         << "Uses the Wt C++ Web Toolkit for browser-based access."
         << std::endl << std::endl
         << "Usage: ores.wt [options] [-- wt-options]"
         << std::endl << std::endl
         << od << std::endl
         << "Wt-specific options (after --):" << std::endl
         << "  --docroot <path>       Document root for static files" << std::endl
         << "  --http-address <addr>  HTTP listen address (default: 0.0.0.0)" << std::endl
         << "  --http-port <port>     HTTP listen port (default: 8080)" << std::endl
         << std::endl;
}

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

ores::wt::config::parser::parse_result
parse_arguments(const std::vector<std::string>& arguments, std::ostream& info) {
    using ores::database::database_configuration;
    using ores::telemetry::log::logging_configuration;
    using ores::utility::program_options::environment_mapper_factory;

    const auto od(make_options_description());
    const auto name_mapper(environment_mapper_factory::make_mapper("WT"));

    std::vector<std::string> our_args;
    std::vector<std::string> wt_args;

    bool found_separator = false;
    for (const auto& arg : arguments) {
        if (arg == "--") {
            found_separator = true;
            continue;
        }
        if (found_separator) {
            wt_args.push_back(arg);
        } else {
            our_args.push_back(arg);
        }
    }

    variables_map vm;
    boost::program_options::store(
        boost::program_options::command_line_parser(our_args)
            .options(od)
            .allow_unregistered()
            .run(), vm);
    boost::program_options::store(
        boost::program_options::parse_environment(od, name_mapper), vm);

    const bool has_version(vm.count(version_arg) != 0);
    const bool has_help(vm.count(help_arg) != 0);

    if (has_help) {
        print_help(od, info);
        return {{}, {}};
    }

    if (has_version) {
        version(info);
        return {{}, {}};
    }

    options r;
    r.logging = logging_configuration::read_options(vm);
    r.database = database_configuration::read_options(vm);

    return {r, wt_args};
}

}

namespace ores::wt::config {

parser::parse_result
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
