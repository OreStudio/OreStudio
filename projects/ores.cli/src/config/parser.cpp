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
#include "ores.cli/config/parser.hpp"

#include <format>
#include <ostream>
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include <magic_enum/magic_enum.hpp>
#include "ores.utility/version/version.hpp"
#include "ores.utility/program_options/common_configuration.hpp"
#include "ores.logging/logging_configuration.hpp"
#include "ores.cli/config/parser_exception.hpp"
#include "ores.cli/config/entity_parsers/currencies_parser.hpp"
#include "ores.cli/config/entity_parsers/accounts_parser.hpp"
#include "ores.cli/config/entity_parsers/feature_flags_parser.hpp"
#include "ores.cli/config/entity_parsers/login_info_parser.hpp"
#include "ores.cli/config/entity_parsers/roles_parser.hpp"
#include "ores.cli/config/entity_parsers/permissions_parser.hpp"
#include "ores.cli/config/entity_parsers/countries_parser.hpp"
#include "ores.cli/config/entity_parsers/change_reasons_parser.hpp"
#include "ores.cli/config/entity_parsers/change_reason_categories_parser.hpp"

namespace {

const std::string indent("   ");
const std::string more_information("Try --help' for more information.");
const std::string product_version("CLI for ORE Studio v" ORES_VERSION);
const std::string build_info(ORES_BUILD_INFO);
const std::string usage_error_msg("Usage error: ");
const std::string no_command_msg("No command supplied. ");


const std::string currencies_command_name("currencies");
const std::string currencies_command_desc("Manage currencies (import, export, list, delete, add).");

const std::string accounts_command_name("accounts");
const std::string accounts_command_desc("Manage accounts (list, delete, add).");

const std::string feature_flags_command_name("feature-flags");
const std::string feature_flags_command_desc("Manage feature flags (list, delete, add).");

const std::string login_info_command_name("login-info");
const std::string login_info_command_desc("View login tracking information (list).");

const std::string roles_command_name("roles");
const std::string roles_command_desc("Manage roles (list, delete).");

const std::string permissions_command_name("permissions");
const std::string permissions_command_desc("Manage permissions (list, delete).");

const std::string countries_command_name("countries");
const std::string countries_command_desc("Manage countries (list, delete).");

const std::string change_reasons_command_name("change-reasons");
const std::string change_reasons_command_desc("Manage change reasons (list, delete).");

const std::string change_reason_categories_command_name("change-reason-categories");
const std::string change_reason_categories_command_desc("Manage change reason categories (list, delete).");

const std::string operation_arg("operation");

const std::string help_arg("help");
const std::string version_arg("version");
const std::string command_arg("command");

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::parsed_options;
using boost::program_options::options_description;
using boost::program_options::positional_options_description;

using ores::cli::config::options;
using ores::cli::config::parser_exception;
namespace entity_parsers = ores::cli::config::entity_parsers;

/**
 * @brief Creates the the top-level option descriptions that are visible to the
 * end users.
 */
options_description make_top_level_visible_options_description() {
    using ores::logging::logging_configuration;
    using ores::utility::program_options::common_configuration;

    const auto god(common_configuration::make_options_description());
    const auto lod(logging_configuration::make_options_description("ores.cli.log"));

    options_description r;
    r.add(god).add(lod);
    return r;
}

/**
 * @brief Creates the the top-level option descriptions that are hidden to end
 * users.
 */
options_description make_top_level_hidden_options_description() {
    options_description r("Commands");
    r.add_options()
        ("command", value<std::string>(), "Command to execute. "
            "Available commands: import.")
        ("args", value<std::vector<std::string> >(),
            "Arguments for command");
    return r;
}

/**
 * @brief Creates the positional options.
 */
positional_options_description make_positional_options() {
    positional_options_description r;
    r.add("command", 1).add("args", -1);
    return r;
}


/**
 * @brief Ensures the supplied command is a valid command.
 */
void validate_command_name(const std::string& command_name) {
    const bool is_valid_command_name(
        command_name == currencies_command_name ||
        command_name == accounts_command_name ||
        command_name == feature_flags_command_name ||
        command_name == login_info_command_name ||
        command_name == roles_command_name ||
        command_name == permissions_command_name ||
        command_name == countries_command_name ||
        command_name == change_reasons_command_name ||
        command_name == change_reason_categories_command_name);

    if (!is_valid_command_name)
    {
        BOOST_THROW_EXCEPTION(parser_exception(
                std::format("Invalid or unsupported command: {}. "
                    "Available commands: currencies, accounts, feature-flags, login-info, "
                    "roles, permissions, countries, change-reasons, change-reason-categories",
                    command_name)));
    }
}

/**
 * @brief Prints the header of the help text, applicable to all cases.
 */
void print_help_header(std::ostream& s) {
    s << "ORE Studio is a User Interface for Open Source Risk Engine (ORE)."
      << std::endl
      << "CLI provides a command line version of the interface." << std::endl
      << "ORE Studio is created by the ORE Studio project. " << std::endl;
}

/**
 * @brief Prints the top-level help text when no command is supplied.
 *
 * @param od top-level options.
 * @param info information stream.
 */
void print_help(const options_description& od, std::ostream& info) {
    print_help_header(info);
    info << "ores.cli uses a command-based interface: <command> <options>. "
         << std::endl << "See below for a list of valid commands. " << std::endl
         << std::endl << "Global options: " << std::endl << od << std::endl
         <<  "Commands: "<< std::endl << std::endl;

    auto lambda([&](const std::string& name, const std::string& desc) {
        const unsigned int command_width(15);
        info << indent << std::setfill(' ') << std::left
             << std::setw(command_width)
             << name << desc << std::endl;
    });

    lambda(currencies_command_name, currencies_command_desc);
    lambda(accounts_command_name, accounts_command_desc);
    lambda(feature_flags_command_name, feature_flags_command_desc);
    lambda(login_info_command_name, login_info_command_desc);
    lambda(roles_command_name, roles_command_desc);
    lambda(permissions_command_name, permissions_command_desc);
    lambda(countries_command_name, countries_command_desc);
    lambda(change_reasons_command_name, change_reasons_command_desc);
    lambda(change_reason_categories_command_name, change_reason_categories_command_desc);

    info << std::endl << "For entity and operation specific options, use: <entity> <operation> --help"
         << std::endl;
}

/**
 * @brief Print the program's version details.
 *
 * @param info information stream.
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
 * @brief Contains the processing logic for when the user did not supply a
 * command in the command line.
 */
std::optional<options>
handle_no_command(const bool has_version, const bool has_help,
    const options_description& od, std::ostream& info) {
    /*
     * The only valid options are help or version, so if those are not present
     * we can safely throw.
     */
    if (!has_version && !has_help)
        BOOST_THROW_EXCEPTION(parser_exception(no_command_msg));

    /*
     * Note that we do not mind if the user has supplied both help and version -
     * help takes priority.
     */
    if (has_help)
        print_help(od, info);
    else if (has_version)
        version(info);

    return {};
}


/**
 * @brief Contains the processing logic for when the user supplies a command in
 * the command line.
 */
std::optional<options>
handle_command(const std::string& command_name, const bool has_help,
    const parsed_options& po, std::ostream& info, variables_map& vm) {

    // All entity commands are handled by dedicated parsers
    if (command_name == currencies_command_name) {
        return entity_parsers::handle_currencies_command(has_help, po, info, vm);
    } else if (command_name == accounts_command_name) {
        return entity_parsers::handle_accounts_command(has_help, po, info, vm);
    } else if (command_name == feature_flags_command_name) {
        return entity_parsers::handle_feature_flags_command(has_help, po, info, vm);
    } else if (command_name == login_info_command_name) {
        return entity_parsers::handle_login_info_command(has_help, po, info, vm);
    } else if (command_name == roles_command_name) {
        return entity_parsers::handle_roles_command(has_help, po, info, vm);
    } else if (command_name == permissions_command_name) {
        return entity_parsers::handle_permissions_command(has_help, po, info, vm);
    } else if (command_name == countries_command_name) {
        return entity_parsers::handle_countries_command(has_help, po, info, vm);
    } else if (command_name == change_reasons_command_name) {
        return entity_parsers::handle_change_reasons_command(has_help, po, info, vm);
    } else if (command_name == change_reason_categories_command_name) {
        return entity_parsers::handle_change_reason_categories_command(has_help, po, info, vm);
    }

    // Unreachable - all commands handled above
    return {};
}

/**
 * @brief Parses the arguments supplied in the command line and converts them
 * into a configuration object.
 */
std::optional<options>
parse_arguments(const std::vector<std::string>& arguments, std::ostream& info) {
    /*
     * Create the top-level command line options, parse them and retrieve the
     * results of the parsing. Note that we split then into visible and hidden
     * to avoid showing the hidden options to the user in the help description.
     */
    const auto visible(make_top_level_visible_options_description());
    const auto hidden(make_top_level_hidden_options_description());
    const auto all(
        [&](){
            options_description r;
            r.add(visible).add(hidden);
            return r;
        }());

    const auto po = boost::program_options::command_line_parser(arguments).
        options(all).
        positional(make_positional_options()).
        allow_unregistered().
        run();

    variables_map vm;
    boost::program_options::store(po, vm);
    const bool has_command(vm.count(command_arg) != 0);
    const bool has_version(vm.count(version_arg) != 0);
    const bool has_help(vm.count(help_arg) != 0);

    /*
     * First, handle the simpler case where no command is supplied. Note that we
     * only supply the visible options here.
     */
    if (!has_command)
        return handle_no_command(has_version, has_help, visible, info);

    /*
     * If the user supplied a command, we need to retrieve it and ensure it is
     * valid.
     */
    const auto command_name(vm[command_arg].as<std::string>());
    validate_command_name(command_name);

    /*
     * Copying the same approach as git, we also consider version to be invalid
     * at the command level. We don't bother to handle this at program options
     * level, but instead check for the presence of the (supposedly valid,
     * according to program options) version command and throw.
     */
    if (has_version)
        BOOST_THROW_EXCEPTION(parser_exception(
                "Option is not valid for command: 'version'."));

    /*
     * We can now process the command. Notice that we are supplying the
     * variables map into the handler by reference. This is because we need
     * access to the global options that may have already been setup.
     */
    return handle_command(command_name, has_help, po, info, vm);
}

}

namespace ores::cli::config {

std::optional<options>
parser::parse(const std::vector<std::string>& arguments,
    std::ostream& info, std::ostream& err) const {

    try {
        return parse_arguments(arguments, info);
    } catch(const parser_exception& e) {
        err << usage_error_msg << e.what() << std::endl
            << more_information << std::endl;
        BOOST_THROW_EXCEPTION(e);
    } catch (const boost::program_options::unknown_option& e) {
        std::string msg = e.what();
        msg += ": '" + e.get_option_name() + "'";
        err << usage_error_msg << msg << std::endl
            << more_information << std::endl;
        BOOST_THROW_EXCEPTION(parser_exception(msg));
    } catch (const boost::program_options::error& e) {
        err << usage_error_msg << e.what() << std::endl
            << more_information << std::endl;
        BOOST_THROW_EXCEPTION(parser_exception(e.what()));
    }
}

}
