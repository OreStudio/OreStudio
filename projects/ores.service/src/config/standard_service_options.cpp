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
#include "ores.service/config/standard_service_options.hpp"
#include "ores.database/config/database_configuration.hpp"
#include "ores.logging/logging_configuration.hpp"
#include "ores.nats/config/nats_configuration.hpp"
#include "ores.utility/program_options/common_configuration.hpp"
#include "ores.utility/program_options/environment_mapper_factory.hpp"

namespace ores::service::config {

namespace {

const std::string help_arg("help");
const std::string version_arg("version");

}

boost::program_options::options_description standard_service_options::make_options_description(
    const std::string& log_file, const boost::program_options::options_description& extra_options) {
    using ores::database::database_configuration;
    using ores::logging::logging_configuration;
    using ores::nats::config::nats_configuration;
    using ores::utility::program_options::common_configuration;

    boost::program_options::options_description r;
    r.add(common_configuration::make_options_description());
    r.add(logging_configuration::make_options_description(log_file));
    r.add(database_configuration::make_options_description());
    r.add(nats_configuration::make_options_description());
    nats_configuration::register_shared_domain();

    if (!extra_options.options().empty())
        r.add(extra_options);

    return r;
}

boost::program_options::variables_map
standard_service_options::parse(const boost::program_options::options_description& od,
                                const std::vector<std::string>& arguments,
                                const std::string& app_name) {
    using ores::utility::program_options::environment_mapper_factory;
    const auto name_mapper(environment_mapper_factory::make_mapper(app_name));

    boost::program_options::variables_map vm;
    boost::program_options::store(
        boost::program_options::command_line_parser(arguments).options(od).run(), vm);
    boost::program_options::store(boost::program_options::parse_environment(od, name_mapper), vm);
    return vm;
}

standard_options
standard_service_options::read_options(const boost::program_options::variables_map& vm) {
    using ores::database::database_configuration;
    using ores::logging::logging_configuration;
    using ores::nats::config::nats_configuration;

    standard_options r;
    r.logging = logging_configuration::read_options(vm);
    r.nats = nats_configuration::read_options(vm);
    r.database = database_configuration::read_options(vm);
    return r;
}

bool standard_service_options::wants_help(const boost::program_options::variables_map& vm) {
    return vm.count(help_arg) != 0;
}

bool standard_service_options::wants_version(const boost::program_options::variables_map& vm) {
    return vm.count(version_arg) != 0;
}

}
