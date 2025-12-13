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
#include "ores.cli/config/entity_parsers/login_info_parser.hpp"

#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include "ores.cli/config/parser_helpers.hpp"
#include "ores.cli/config/parser_exception.hpp"
#include "ores.cli/config/entity.hpp"
#include "ores.cli/config/add_login_info_options.hpp"
#include "ores.database/config/database_configuration.hpp"
#include "ores.utility/log/logging_configuration.hpp"
#include "ores.utility/program_options/environment_mapper_factory.hpp"

namespace ores::cli::config::entity_parsers {

namespace {

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::parsed_options;
using boost::program_options::options_description;
using boost::program_options::command_line_parser;
using boost::program_options::parse_environment;
using boost::program_options::include_positional;
using boost::program_options::collect_unrecognized;

using ores::cli::config::entity;
using ores::cli::config::options;
using ores::cli::config::parser_exception;
using ores::cli::config::parser_helpers::print_help_command;
using ores::cli::config::parser_helpers::add_common_options;
using ores::cli::config::parser_helpers::validate_operation;
using ores::cli::config::parser_helpers::print_entity_help;
using ores::cli::config::parser_helpers::make_export_options_description;
using ores::cli::config::parser_helpers::make_delete_options_description;
using ores::cli::config::parser_helpers::read_export_options;
using ores::cli::config::parser_helpers::read_delete_options;

const std::string delete_command_name("delete");
const std::string list_command_name("list");
const std::string add_command_name("add");

const std::vector<std::string> allowed_operations{
    list_command_name, delete_command_name, add_command_name
};

/**
 * @brief Creates the options related to adding login info.
 */
options_description make_add_login_info_options_description() {
    options_description r("Add Login Info Options");
    r.add_options()
        ("account-id",
            value<std::string>(),
            "Account ID (UUID) for the login info (required)")
        ("locked",
            value<bool>()->default_value(false),
            "Whether the account is locked (default: false)")
        ("failed-logins",
            value<int>()->default_value(0),
            "Number of failed login attempts (default: 0)");

    return r;
}

/**
 * @brief Reads the add configuration from the variables map for login info.
 */
ores::cli::config::add_login_info_options
read_add_login_info_options(const variables_map& vm) {
    ores::cli::config::add_login_info_options r;

    // Login info requires account-id
    if (vm.count("account-id") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --account-id for add login-info command."));
    }
    r.account_id = vm["account-id"].as<std::string>();

    // Optional fields with defaults
    if (vm.count("locked") != 0)
        r.locked = vm["locked"].as<bool>();
    if (vm.count("failed-logins") != 0)
        r.failed_logins = vm["failed-logins"].as<int>();

    return r;
}

}

std::optional<options>
handle_login_info_command(bool has_help,
    const parsed_options& po,
    std::ostream& info,
    variables_map& vm) {

    // Collect all unrecognized options from the first pass
    auto o(collect_unrecognized(po.options, include_positional));
    o.erase(o.begin()); // Remove command name

    // Show help for login-info command if requested with no operation
    if (has_help && o.empty()) {
        const std::vector<std::pair<std::string, std::string>> operations = {
            {"list", "List login info records as JSON or table"},
            {"delete", "Delete a login info record by account ID"},
            {"add", "Add a new login info record"}
        };
        print_entity_help("login-info", "Manage login tracking information", operations, info);
        return {};
    }

    if (o.empty()) {
        BOOST_THROW_EXCEPTION(parser_exception(
            "login-info command requires an operation (list, delete, add)"));
    }

    const auto operation = o.front();
    o.erase(o.begin()); // Remove operation from args

    // Validate operation
    validate_operation("login-info", operation, allowed_operations);

    options r;
    using ores::utility::program_options::environment_mapper_factory;
    const auto name_mapper(environment_mapper_factory::make_mapper("CLI"));

    if (operation == list_command_name) {
        auto d = add_common_options(make_export_options_description());
        if (has_help) {
            print_help_command("login-info list", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        r.exporting = read_export_options(vm, entity::login_info);
    } else if (operation == delete_command_name) {
        auto d = add_common_options(make_delete_options_description());
        if (has_help) {
            print_help_command("login-info delete", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        r.deleting = read_delete_options(vm, entity::login_info);
    } else if (operation == add_command_name) {
        auto d = add_common_options(make_add_login_info_options_description());
        if (has_help) {
            print_help_command("login-info add", d, info);
            return {};
        }
        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        r.adding = read_add_login_info_options(vm);
    }

    // Read common options
    using ores::database::database_configuration;
    using ores::utility::log::logging_configuration;
    r.database = database_configuration::read_options(vm);
    r.logging = logging_configuration::read_options(vm);

    return r;
}

}
