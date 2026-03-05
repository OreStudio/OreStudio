/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.mq.broker/config/parser.hpp"

#include <ostream>
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include "ores.utility/version/version.hpp"
#include "ores.utility/program_options/common_configuration.hpp"
#include "ores.utility/program_options/environment_mapper_factory.hpp"
#include "ores.logging/logging_configuration.hpp"
#include "ores.mq.broker/config/parser_exception.hpp"

namespace {

const std::string more_information("Try '--help' for more information.");
const std::string product_version("OreStudio MQ Broker v" ORES_VERSION);
const std::string build_info(ORES_BUILD_INFO);
const std::string usage_error_msg("Usage error: ");

const std::string help_arg("help");
const std::string version_arg("version");
const std::string frontend_arg("frontend");
const std::string backend_arg("backend");

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::options_description;

using ores::mq::broker::config::options;
using ores::mq::broker::config::parser_exception;

options_description make_options_description() {
    using ores::logging::logging_configuration;
    using ores::utility::program_options::common_configuration;

    const auto god(common_configuration::make_options_description());
    const auto lod(logging_configuration::make_options_description(
        "ores.mq.broker.log"));

    options_description broker_opts("Broker options");
    broker_opts.add_options()
        (frontend_arg.c_str(),
         value<std::string>()->default_value("tcp://0.0.0.0:7001")
             ->value_name("URL"),
         "NNG endpoint for client connections (frontend socket)")
        (backend_arg.c_str(),
         value<std::string>()->default_value("tcp://0.0.0.0:7002")
             ->value_name("URL"),
         "NNG endpoint for service connections (backend socket)");

    options_description r;
    r.add(god).add(lod).add(broker_opts);
    return r;
}

void print_help(const options_description& od, std::ostream& info) {
    info << "OreStudio MQ Broker routes binary protocol frames to registered services."
         << std::endl << std::endl
         << "Usage: ores.mq.broker [options]"
         << std::endl << std::endl
         << od << std::endl;
}

void print_version(std::ostream& info) {
    info << product_version << std::endl
         << "Copyright (C) 2026 Marco Craveiro." << std::endl
         << "License GPLv3: GNU GPL version 3 or later "
         << "<http://gnu.org/licenses/gpl.html>." << std::endl;
    if (!build_info.empty())
        info << build_info << std::endl;
}

std::optional<options>
parse_arguments(const std::vector<std::string>& arguments, std::ostream& info) {
    using ores::logging::logging_configuration;
    using ores::utility::program_options::environment_mapper_factory;

    const auto od(make_options_description());
    const auto name_mapper(environment_mapper_factory::make_mapper("BROKER"));

    variables_map vm;
    boost::program_options::store(
        boost::program_options::command_line_parser(arguments).
        options(od).run(), vm);
    boost::program_options::store(
        boost::program_options::parse_environment(od, name_mapper), vm);

    if (vm.count(help_arg)) {
        print_help(od, info);
        return {};
    }
    if (vm.count(version_arg)) {
        print_version(info);
        return {};
    }

    options r;
    r.logging = logging_configuration::read_options(vm);
    r.frontend_endpoint = vm[frontend_arg].as<std::string>();
    r.backend_endpoint  = vm[backend_arg].as<std::string>();
    return r;
}

}

namespace ores::mq::broker::config {

std::optional<options>
parser::parse(const std::vector<std::string>& arguments,
    std::ostream& info, std::ostream& err) const {

    try {
        return parse_arguments(arguments, info);
    } catch (const parser_exception& e) {
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
