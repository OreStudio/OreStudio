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
#include "ores.database/config/database_configuration.hpp"

#include <regex>
#include <ranges>
#include <boost/throw_exception.hpp>
#include "ores.platform/environment/environment.hpp"

namespace ores::database {

using ores::platform::environment::environment;

namespace {

const std::string database_user_arg("db-user");
const std::string database_password_arg("db-password");
const std::string database_host_arg("db-host");
const std::string database_database_arg("db-database");
const std::string database_port_arg("db-port");
const std::string database_tenant_arg("tenant");

}

boost::program_options::options_description
database_configuration::make_options_description() {
    using boost::program_options::value;
    using boost::program_options::options_description;

    options_description r("Database");
    r.add_options()
        ("db-user",
            value<std::string>()->default_value("ores_cli_user"),
            "Database user name.")
        ("db-password",
            value<std::string>()->default_value(""),
            "Database password. Also reads from ORES_DB_<APP>_PASSWORD env var "
            "(e.g., ORES_DB_CLI_PASSWORD for ores_cli_user).")
        ("db-host",
            value<std::string>()->default_value("localhost"),
            "Database host.")
        ("db-database",
            value<std::string>()->default_value("ores_default"),
            "Database name. Convention: ores_dev_<env> (e.g., ores_dev_local2).")
        ("db-port",
            value<int>()->default_value(5432),
            "Database port.")
        ("tenant",
            value<std::string>()->default_value(""),
            "Tenant code (e.g., 'system', 'acme') or tenant UUID. "
            "Required for multi-tenant database operations. "
            "Also reads from ORES_TENANT env var.");

    return r;
}

database_options database_configuration::read_options(
    const boost::program_options::variables_map& vm) {
    database_options r;

    r.user = vm[database_user_arg].as<std::string>();
    r.database = vm[database_database_arg].as<std::string>();
    r.host = vm[database_host_arg].as<std::string>();
    r.port = vm[database_port_arg].as<int>();

    // Get password from command line, or fall back to environment variable.
    // Derive env var name from user: ores_<app>_user -> ORES_DB_<APP>_PASSWORD
    auto password = vm[database_password_arg].as<std::string>();
    if (password.empty()) {
        std::regex user_pattern("^ores_([a-z]+)_user$");
        std::smatch match;
        if (std::regex_match(r.user, match, user_pattern)) {
            std::string app = match[1].str();
            std::ranges::transform(app, app.begin(), ::toupper);
            const std::string env_var = "ORES_DB_" + app + "_PASSWORD";
            password = environment::get_value_or_default(env_var, "");
        }
    }
    r.password = password;

    // Get tenant from command line, or fall back to ORES_TENANT env var
    auto tenant = vm[database_tenant_arg].as<std::string>();
    if (tenant.empty()) {
        tenant = environment::get_value_or_default("ORES_TENANT", "");
    }
    r.tenant = tenant;

    return r;
}

}
