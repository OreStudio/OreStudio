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
#include <ranges>
#include <algorithm>
#include <boost/throw_exception.hpp>
#include "ores.utility/database/database_exception.hpp"
#include "ores.utility/database/database_configuration.hpp"

namespace ores::utility::database {

namespace {

const std::string database_user_arg("db-user");
const std::string database_password_arg("db-password");
const std::string database_host_arg("db-host");
const std::string database_database_arg("db-database");
const std::string database_port_arg("db-port");

}

boost::program_options::options_description
database_configuration::make_options_description() {
    using boost::program_options::value;
    using boost::program_options::options_description;

    options_description r("Database");
    r.add_options()
        ("db-user",
            value<std::string>(),
            "Database user name.")
        ("db-password",
            value<std::string>(),
            "Database password. Can also be provided via ORES_DB_PASSWORD environment variable.")
        ("db-host",
            value<std::string>()->default_value("localhost"),
            "Database host. Defaults to localhost.")
        ("db-database",
            value<std::string>(),
            "Database name.")
        ("db-port",
            value<int>()->default_value(5432),
            "Database port. Defaults to 5432.");

    return r;
}

std::optional<database_options>
database_configuration::read_options(
    const boost::program_options::variables_map& vm) {
    const bool has_user(vm.count(database_user_arg) != 0);
    const bool has_password(vm.count(database_password_arg) != 0);
    const bool has_database(vm.count(database_database_arg) != 0);
    const bool has_host(vm.count(database_host_arg) != 0);
    const bool has_port(vm.count(database_port_arg) != 0);

    if (!has_user && !has_password && !has_database && !has_host && !has_port)
        return {};

    database_options r;

    if (has_user)
        r.user = vm[database_user_arg].as<std::string>();
    else
        r.user = "ores";

    if (has_password)
        r.password = vm[database_password_arg].as<std::string>();
    else
        BOOST_THROW_EXCEPTION(database_exception(
            "Must supply database password via --db-password or ORES_DB_PASSWORD environment variable."));

    if (has_database)
        r.database = vm[database_database_arg].as<std::string>();
    else
        r.database = "oresdb";

    if (has_host)
        r.host = vm[database_host_arg].as<std::string>();
    else
        r.host = "localhost";

    if (has_port)
        r.port = vm[database_port_arg].as<int>();
    else
        r.port = 5432;

    return r;
}

std::function<std::string(const std::string&)>
database_configuration::make_environment_mapper() {
    return [](const std::string& env_var) -> std::string {
        constexpr std::string_view prefix = "ORES_";
        if (!env_var.starts_with(prefix)) {
            return {};
        }

        auto env_body = env_var | std::views::drop(prefix.size());
        std::string option_name;

        std::ranges::transform(env_body, std::back_inserter(option_name),
            [](unsigned char c) -> char {
                if (c == '_') return '-';
                return std::tolower(c);
            });

        return option_name;
    };
}

}
