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
#include "ores.utility/database/database_configuration.hpp"

#include <boost/throw_exception.hpp>

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
            value<std::string>()->default_value("ores"),
            "Database user name.")
        ("db-password",
            value<std::string>()->default_value(""),
            "Database password. Can also be provided via ORES_DB_PASSWORD environment variable.")
        ("db-host",
            value<std::string>()->default_value("localhost"),
            "Database host.")
        ("db-database",
            value<std::string>()->default_value("oresdb"),
            "Database name.")
        ("db-port",
            value<int>()->default_value(5432),
            "Database port.");

    return r;
}

database_options database_configuration::read_options(
    const boost::program_options::variables_map& vm) {
    database_options r;

    r.user = vm[database_user_arg].as<std::string>();
    r.password = vm[database_password_arg].as<std::string>();
    r.database = vm[database_database_arg].as<std::string>();
    r.host = vm[database_host_arg].as<std::string>();
    r.port = vm[database_port_arg].as<int>();
    return r;
}

}
