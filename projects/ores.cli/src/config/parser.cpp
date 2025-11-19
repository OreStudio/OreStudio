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
#include "ores.cli/config/format.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.utility/log/logging_configuration.hpp"
#include "ores.utility/database/database_configuration.hpp"
#include "ores.utility/program_options/environment_mapper_factory.hpp"
#include "ores.cli/config/entity.hpp"
#include "ores.cli/config/parser_exception.hpp"
#include "ores.cli/config/entity_parsers/currencies_parser.hpp"
#include "ores.cli/config/entity_parsers/accounts_parser.hpp"

namespace {

const std::string indent("   ");
const std::string more_information("Try --help' for more information.");
const std::string product_version("CLI for ORE Studio v" ORES_VERSION);
const std::string build_info(ORES_BUILD_INFO);
const std::string usage_error_msg("Usage error: ");
const std::string no_command_msg("No command supplied. ");

const std::string entity_arg("entity");
const std::string import_targets_arg("target");
const std::string export_as_of_arg("as-of");
const std::string export_key_arg("key");
const std::string export_all_versions_arg("all-versions");
const std::string export_format_arg("format");
const std::string delete_key_arg("key");

const std::string import_command_name("import");
const std::string export_command_name("export");
const std::string delete_command_name("delete");

const std::string currencies_command_name("currencies");
const std::string currencies_command_desc("Manage currencies (import, export, list, delete, add).");

const std::string accounts_command_name("accounts");
const std::string accounts_command_desc("Manage accounts (list, delete, add).");

const std::string feature_flags_command_name("feature_flags");
const std::string feature_flags_command_desc("Manage feature flags (list, delete, add).");

const std::string operation_arg("operation");

const std::string help_arg("help");
const std::string version_arg("version");
const std::string command_arg("command");

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::parsed_options;
using boost::program_options::options_description;
using boost::program_options::positional_options_description;

using ores::cli::config::entity;
using ores::cli::config::format;
using ores::cli::config::options;
using ores::cli::config::import_options;
using ores::cli::config::export_options;
using ores::cli::config::parser_exception;
namespace entity_parsers = ores::cli::config::entity_parsers;

/**
 * @brief Creates the the top-level option descriptions that are visible to the
 * end users.
 */
options_description make_top_level_visible_options_description() {
    using ores::utility::log::logging_configuration;

    options_description god("General");
    god.add_options()
        ("help,h", "Display usage and exit.")
        ("version,v", "Output version information and exit.");

    options_description r;
    r.add(god);

    const auto lod(logging_configuration::make_options_description("ores.cli.log"));
    r.add(lod);
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
 * @brief Creates the options related to importing.
 */
options_description make_import_options_description() {
    options_description r("Import");
    r.add_options()
        ("entity",
            value<std::string>(),
            "Entity to import, e.g. 'currency_config', etc.")
        ("target",
            value<std::vector<std::string>>(),
            "One or more target files containing entities.");

    return r;
}

/**
 * @brief Creates the options related to ORE exporting.
 */
options_description make_export_options_description() {
    options_description r("Export");
    r.add_options()
        ("entity",
            value<std::string>(),
            "Entity to export, e.g. 'currency_config', etc.")
        ("as-of", value<std::string>(),
            "Time point from which to dump data. If not supplied, defaults to latest.")
        ("key", value<std::string>(), "Key to filter data by.")
        ("all-versions", "If supplied, retrieves all versions.")
        ("format", value<std::string>(), "Format to export data in, e.g. xml or json.");

    return r;
}

/**
 * @brief Creates the options related to deleting entities.
 */
options_description make_delete_options_description() {
    options_description r("Delete");
    r.add_options()
        ("entity",
            value<std::string>(),
            "Entity to delete, e.g. 'accounts', etc.")
        ("key", value<std::string>(), "Key to identify the entity (e.g., account ID or username).");

    return r;
}

/**
 * @brief Creates the options related to adding currencies.
 */
options_description make_add_currency_options_description() {
    options_description r("Add Currency Options");
    r.add_options()
        ("iso-code",
            value<std::string>(),
            "Currency ISO code (required, e.g., USD)")
        ("name",
            value<std::string>(),
            "Currency name (required, e.g., 'United States Dollar')")
        ("numeric-code",
            value<std::string>()->default_value("0"),
            "Currency numeric code")
        ("symbol",
            value<std::string>()->default_value(""),
            "Currency symbol")
        ("fraction-symbol",
            value<std::string>()->default_value(""),
            "Fraction symbol")
        ("fractions-per-unit",
            value<int>()->default_value(100),
            "Fractions per unit")
        ("rounding-type",
            value<std::string>()->default_value("Closest"),
            "Rounding type")
        ("rounding-precision",
            value<int>()->default_value(2),
            "Rounding precision")
        ("format",
            value<std::string>()->default_value(""),
            "Display format")
        ("currency-type",
            value<std::string>()->default_value(""),
            "Currency type")
        ("modified-by",
            value<std::string>(),
            "Username of modifier (required)");

    return r;
}

/**
 * @brief Creates the options related to adding accounts.
 */
options_description make_add_account_options_description() {
    options_description r("Add Account Options");
    r.add_options()
        ("username",
            value<std::string>(),
            "Account username (required)")
        ("email",
            value<std::string>(),
            "Account email address (required)")
        ("password",
            value<std::string>(),
            "Account password (required)")
        ("is-admin",
            value<bool>()->default_value(false),
            "Whether the account has admin privileges (default: false)")
        ("modified-by",
            value<std::string>(),
            "Username of modifier (required)");

    return r;
}

/**
 * @brief Creates the options related to adding feature flags.
 */
options_description make_add_feature_flag_options_description() {
    options_description r("Add Feature Flag Options");
    r.add_options()
        ("name",
            value<std::string>(),
            "Feature flag name (required)")
        ("description",
            value<std::string>()->default_value(""),
            "Feature flag description")
        ("enabled",
            value<bool>()->default_value(false),
            "Whether the feature is enabled (default: false)")
        ("modified-by",
            value<std::string>(),
            "Username of modifier (required)");

    return r;
}

/**
 * @brief Creates the options related to client testing.
 */
options_description make_client_options_description() {
    options_description r("Client");
    // No options needed - connection details are provided in REPL
    return r;
}


/**
 * @brief Ensures the supplied command is a valid command.
 */
void validate_command_name(const std::string& command_name) {
    const bool is_valid_command_name(
        command_name == currencies_command_name ||
        command_name == accounts_command_name ||
        command_name == feature_flags_command_name);

    if (!is_valid_command_name)
    {
        BOOST_THROW_EXCEPTION(parser_exception(
                std::format("Invalid or unsupported command: {}. "
                    "Available commands: currencies, accounts, feature_flags",
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

    info << std::endl << "For entity and operation specific options, use: <entity> <operation> --help"
         << std::endl;
}

/**
 * @brief Prints help text at the command level.
 *
 * @param command_name name of the command to print help for.
 * @param od command options.
 * @param info information stream.
 */
void print_help_command(const std::string& command_name,
    const options_description& od, std::ostream& info) {
    print_help_header(info);
    info << "Displaying options specific to the '" << command_name << "' command. "
         << std::endl
         << "For global options, type --help." << std::endl << std::endl
         << od;
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
 * @brief Reads entity from the variables map.
 */
entity read_entity(const variables_map& vm) {
    if (vm.count(entity_arg) == 0)
        BOOST_THROW_EXCEPTION(parser_exception("Must supply entity."));

    const auto s(vm[entity_arg].as<std::string>());
    auto e = magic_enum::enum_cast<entity>(s);
    if (e.has_value())
        return e.value();

    BOOST_THROW_EXCEPTION(
        parser_exception("Invalid or unsupported entity: '" + s + "'"));
}

/**
 * @brief Reads format from the variables map.
 */
format read_format(const variables_map& vm) {
    if (vm.count(export_format_arg) == 0)
        return format::json;

    const auto s(vm[export_format_arg].as<std::string>());
    auto f = magic_enum::enum_cast<format>(s);
    if (f.has_value())
        return f.value();

    BOOST_THROW_EXCEPTION(
        parser_exception("Invalid or unsupported format: '" + s + "'"));
}

/**
 * @brief Reads the import configuration from the variables map.
 */
import_options read_import_options(const variables_map& vm) {
    import_options r;

    r.target_entity = read_entity(vm);

    const auto t(vm[import_targets_arg].as<std::vector<std::string>>());
    if (t.empty()) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply at least one import target."));
    }

    r.targets.reserve(t.size());
    using std::filesystem::absolute;
    std::ranges::transform(t, std::back_inserter(r.targets),
        [](const auto& s) { return absolute(s); });
    return r;
}

/**
 * @brief Reads the ore_export configuration from the variables map.
 */
export_options read_export_options(const variables_map& vm) {
    export_options r;

    r.target_entity = read_entity(vm);
    r.target_format = read_format(vm);
    r.all_versions = vm.count(export_all_versions_arg) != 0;

    if (vm.count(export_as_of_arg) != 0)
        r.as_of = vm[export_as_of_arg].as<std::string>();

    if (vm.count(export_key_arg) != 0)
        r.key = vm[export_key_arg].as<std::string>();

    return r;
}


/**
 * @brief Reads the delete configuration from the variables map.
 */
ores::cli::config::delete_options read_delete_options(const variables_map& vm) {
    ores::cli::config::delete_options r;

    r.target_entity = read_entity(vm);

    if (vm.count(delete_key_arg) == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --key argument for delete command."));
    }
    r.key = vm[delete_key_arg].as<std::string>();

    return r;
}

/**
 * @brief Reads the add configuration from the variables map.
 */
ores::cli::config::add_options read_add_options(const variables_map& vm) {
    ores::cli::config::add_options r;

    r.target_entity = read_entity(vm);

    if (vm.count("modified-by") == 0) {
        BOOST_THROW_EXCEPTION(
            parser_exception("Must supply --modified-by for add command."));
    }
    r.modified_by = vm["modified-by"].as<std::string>();

    // Read entity-specific fields based on target entity
    if (r.target_entity == ores::cli::config::entity::currencies) {
        // Currency-specific required fields
        if (vm.count("iso-code") == 0) {
            BOOST_THROW_EXCEPTION(
                parser_exception("Must supply --iso-code for add currency command."));
        }
        r.iso_code = vm["iso-code"].as<std::string>();

        if (vm.count("name") == 0) {
            BOOST_THROW_EXCEPTION(
                parser_exception("Must supply --name for add currency command."));
        }
        r.name = vm["name"].as<std::string>();

        // Optional currency fields with defaults
        if (vm.count("numeric-code") != 0)
            r.numeric_code = vm["numeric-code"].as<std::string>();
        if (vm.count("symbol") != 0)
            r.symbol = vm["symbol"].as<std::string>();
        if (vm.count("fraction-symbol") != 0)
            r.fraction_symbol = vm["fraction-symbol"].as<std::string>();
        if (vm.count("fractions-per-unit") != 0)
            r.fractions_per_unit = vm["fractions-per-unit"].as<int>();
        if (vm.count("rounding-type") != 0)
            r.rounding_type = vm["rounding-type"].as<std::string>();
        if (vm.count("rounding-precision") != 0)
            r.rounding_precision = vm["rounding-precision"].as<int>();
        if (vm.count("format") != 0)
            r.format = vm["format"].as<std::string>();
        if (vm.count("currency-type") != 0)
            r.currency_type = vm["currency-type"].as<std::string>();
    } else if (r.target_entity == ores::cli::config::entity::accounts) {
        // Account-specific required fields
        if (vm.count("username") == 0) {
            BOOST_THROW_EXCEPTION(
                parser_exception("Must supply --username for add account command."));
        }
        r.username = vm["username"].as<std::string>();

        if (vm.count("email") == 0) {
            BOOST_THROW_EXCEPTION(
                parser_exception("Must supply --email for add account command."));
        }
        r.email = vm["email"].as<std::string>();

        if (vm.count("password") == 0) {
            BOOST_THROW_EXCEPTION(
                parser_exception("Must supply --password for add account command."));
        }
        r.password = vm["password"].as<std::string>();

        // Optional account fields with defaults
        if (vm.count("is-admin") != 0)
            r.is_admin = vm["is-admin"].as<bool>();
    } else if (r.target_entity == ores::cli::config::entity::feature_flags) {
        // Feature flag-specific required fields
        if (vm.count("name") == 0) {
            BOOST_THROW_EXCEPTION(
                parser_exception("Must supply --name for add feature flag command."));
        }
        r.flag_name = vm["name"].as<std::string>();

        // Optional feature flag fields with defaults
        if (vm.count("description") != 0)
            r.description = vm["description"].as<std::string>();
        if (vm.count("enabled") != 0)
            r.enabled = vm["enabled"].as<bool>();
    }

    return r;
}

/**
 * @brief Contains the processing logic for when the user supplies a command in
 * the command line.
 */
std::optional<options>
handle_command(const std::string& command_name, const bool has_help,
    const parsed_options& po, std::ostream& info, variables_map& vm) {

    /*
     * Collect all the unrecognized options from the first pass. It includes the
     * positional command name, so we need to erase it.
     */
    using boost::program_options::include_positional;
    using boost::program_options::collect_unrecognized;
    auto o(collect_unrecognized(po.options, include_positional));
    o.erase(o.begin());

    /*
     * For each command we need to setup their set of options, parse them and
     * then generate the appropriate options.
     */
    options r;
    using boost::program_options::command_line_parser;
    using boost::program_options::parse_environment;
    using ores::utility::database::database_configuration;

    const auto db_desc(database_configuration::make_options_description());
    const auto logging_desc(make_top_level_visible_options_description());
    using ores::utility::program_options::environment_mapper_factory;
    const auto name_mapper(environment_mapper_factory::make_mapper("CLI"));

    if (command_name == currencies_command_name) {
        return entity_parsers::handle_currencies_command(has_help, po, info, vm);
    } else if (command_name == accounts_command_name) {
        return entity_parsers::handle_accounts_command(has_help, po, info, vm);
    } else if (command_name == feature_flags_command_name) {
        // Entity-based command: feature_flags <operation> [options]
        if (has_help && o.empty()) {
            // Show help for feature_flags command
            info << "feature_flags - Manage feature flags" << std::endl << std::endl;
            info << "Usage: ores.cli feature_flags <operation> [options]" << std::endl << std::endl;
            info << "Available operations:" << std::endl;
            info << "  list       List feature flags as JSON or table (internal formats)" << std::endl;
            info << "  delete     Delete a feature flag by key" << std::endl;
            info << "  add        Add a new feature flag" << std::endl << std::endl;
            info << "For operation-specific options, use: feature_flags <operation> --help" << std::endl;
            return {};
        }

        if (o.empty()) {
            BOOST_THROW_EXCEPTION(parser_exception(
                "feature_flags command requires an operation (list, delete, add)"));
        }

        const auto operation = o.front();
        o.erase(o.begin()); // Remove operation from args

        if (operation == "list") {
            // List feature_flags operation
            auto d(make_export_options_description()); // Reuse export options for list
            d.add(db_desc).add(logging_desc);
            if (has_help) {
                print_help_command("feature_flags list", d, info);
                return {};
            }
            store(command_line_parser(o).options(d).run(), vm);
            store(parse_environment(d, name_mapper), vm);
            vm.insert(std::make_pair(entity_arg, boost::program_options::variable_value(
                std::string("feature_flags"), false)));
            // Treat list as export for now
            r.exporting = read_export_options(vm);
        } else if (operation == delete_command_name) {
            auto d(make_delete_options_description());
            d.add(db_desc).add(logging_desc);
            if (has_help) {
                print_help_command("feature_flags delete", d, info);
                return {};
            }
            store(command_line_parser(o).options(d).run(), vm);
            store(parse_environment(d, name_mapper), vm);
            vm.insert(std::make_pair(entity_arg, boost::program_options::variable_value(
                std::string("feature_flags"), false)));
            r.deleting = read_delete_options(vm);
        } else if (operation == "add") {
            auto d(make_add_feature_flag_options_description());
            d.add(db_desc).add(logging_desc);
            if (has_help) {
                print_help_command("feature_flags add", d, info);
                return {};
            }
            store(command_line_parser(o).options(d).run(), vm);
            store(parse_environment(d, name_mapper), vm);
            vm.insert(std::make_pair(entity_arg, boost::program_options::variable_value(
                std::string("feature_flags"), false)));
            r.adding = read_add_options(vm);
        } else {
            BOOST_THROW_EXCEPTION(parser_exception(
                std::format("Invalid operation for feature_flags: {}. "
                    "Valid operations: list, delete, add", operation)));
        }
    }

    r.database = database_configuration::read_options(vm);

    /*
     * Now process the common options.
     */
    using ores::utility::log::logging_configuration;
    r.logging = logging_configuration::read_options(vm);
    return r;
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
    } catch (const boost::program_options::error& e) {
        err << usage_error_msg << e.what() << std::endl
            << more_information << std::endl;
        BOOST_THROW_EXCEPTION(parser_exception(e.what()));
    }
}

}
