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
#include "ores.refdata.service/config/parser.hpp"

#include <ostream>
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include "ores.refdata.service/config/parser_exception.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.utility/program_options/common_configuration.hpp"
#include "ores.logging/logging_configuration.hpp"
#include "ores.database/config/database_configuration.hpp"
#include "ores.nats/config/nats_configuration.hpp"
#include "ores.utility/program_options/environment_mapper_factory.hpp"

namespace {

const std::string more_information("Try '--help' for more information.");
const std::string product_version("ores.refdata.service v" ORES_VERSION);
const std::string build_info(ores::utility::version::build_info());
const std::string usage_error_msg("Usage error: ");
const std::string help_arg("help");
const std::string version_arg("version");

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::options_description;

using ores::refdata::service::config::options;
using ores::refdata::service::config::parser_exception;

options_description make_options_description() {
    using ores::database::database_configuration;
    using ores::logging::logging_configuration;
    using ores::utility::program_options::common_configuration;
    using ores::nats::config::nats_configuration;

    options_description r;
    r.add(common_configuration::make_options_description());
    r.add(logging_configuration::make_options_description("ores.refdata.service.log"));
    r.add(database_configuration::make_options_description());
    r.add(nats_configuration::make_options_description());
    return r;
}

void print_help(const options_description& od, std::ostream& info) {
    info << "Reference data service" << std::endl << std::endl
         << "Usage: ores.refdata.service [options]" << std::endl << std::endl
         << od << std::endl;
}

void version(std::ostream& info) {
    info << product_version << std::endl
         << "Copyright (C) 2026 Marco Craveiro." << std::endl
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

std::optional<options>
parse_arguments(const std::vector<std::string>& arguments, std::ostream& info) {
    using ores::database::database_configuration;
    using ores::logging::logging_configuration;
    using ores::nats::config::nats_configuration;

    const auto od(make_options_description());
    using ores::utility::program_options::environment_mapper_factory;
    const auto name_mapper(environment_mapper_factory::make_mapper("REFDATA_SERVICE"));

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
        version(info);
        return {};
    }

    options r;
    r.logging = logging_configuration::read_options(vm);
    r.nats = nats_configuration::read_options(vm);
    r.database = database_configuration::read_options(vm);
    return r;
}

}

namespace ores::refdata::service::config {

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
