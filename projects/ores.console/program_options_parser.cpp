/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#include <iomanip>
#include <iostream>
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include "ores.utility/config/config.hpp"
#include "ores.utility/log/severity_level.hpp"
#include "ores.utility/log/logging_configuration.hpp"
#include "ores.console/parser_exception.hpp"
#include "ores.console/program_options_parser.hpp"

namespace {

const std::string indent("   ");
const std::string more_information("Try --help' for more information.");
const std::string product_version("OreStudio v" ORES_VERSION);
const std::string build_info(ORES_BUILD_INFO);
const std::string usage_error_msg("Usage error: ");
const std::string no_command_msg("No command supplied. ");

const std::string importing_command_name("import");
const std::string importing_command_desc("Imports data into the system.");
const std::string importing_curency_config_arg("currency-configuration");

const std::string help_arg("help");
const std::string version_arg("version");
const std::string command_arg("command");

const std::string logging_log_enabled_arg("log-enabled");
const std::string logging_log_to_console_arg("log-to-console");
const std::string logging_log_level_arg("log-level");
const std::string logging_log_dir_arg("log-directory");
const std::string logging_log_level_trace("trace");
const std::string logging_log_level_debug("debug");
const std::string logging_log_level_info("info");
const std::string logging_log_level_warn("warn");
const std::string logging_log_level_error("error");

const std::string invalid_log_level("Log level is invalid: ");
const std::string invalid_command("Command is invalid or unsupported: ");
const std::string invalid_option("Option is not valid for command: ");
const std::string missing_import_target("Supply at least one import target.");

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::parsed_options;
using boost::program_options::options_description;
using boost::program_options::positional_options_description;

using ores::console::configuration;
using ores::console::parser_exception;
using ores::utility::log::logging_configuration;
using ores::console::importing_configuration;

/**
 * @brief Creates the the top-level option descriptions that are visible to the
 * end users.
 */
options_description make_top_level_visible_options_description() {
    options_description god("General");
    god.add_options()
        ("help,h", "Display usage and exit.")
        ("version,v", "Output version information and exit.");

    options_description r;
    r.add(god);

    options_description lod("Logging");
    lod.add_options()
        ("log-enabled,e", "Generate a log file.")
        ("log-level,l", value<std::string>(),
            "What level to use for logging. Valid values: trace, debug, info, "
            "warn, error. Defaults to info.")
        ("log-to-console",
            "Output logging to the console, as well as to file.")
        ("log-directory", value<std::string>(),
            "Where to place the log files.");
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
options_description make_importing_options_description() {
    options_description r("Importing");
    r.add_options()
        ("currency-configuration",
            value<std::vector<std::string>>(),
            "One or more currency configuration files, in XML representation.");

    return r;
}

/**
 * @brief Ensures the supplied command is a valid command.
 */
void validate_command_name(const std::string& command_name) {
    const bool is_valid_command_name(command_name == importing_command_name);

    if (!is_valid_command_name)
    {
        BOOST_THROW_EXCEPTION(parser_exception(invalid_command + command_name));
    }
}

/**
 * @brief Prints the header of the help text, applicable to all cases.
 */
void print_help_header(std::ostream& s) {
    s << "ORE Studio is a User Interface for Open Source Risk Engine (ORE)."
      << std::endl
      << "Console provides a CLI based version of the interface." << std::endl
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
    info << "ores.console uses a command-based interface: <command> <options>. "
         << std::endl << "See below for a list of valid commands. " << std::endl
         << std::endl << "Global options: " << std::endl << od << std::endl
         <<  "Commands: "<< std::endl << std::endl;

    auto lambda([&](const std::string& name, const std::string& desc) {
        const unsigned int command_width(15);
        info << indent << std::setfill(' ') << std::left
             << std::setw(command_width)
             << name << desc << std::endl;
    });

    lambda(importing_command_name, importing_command_desc);

    info << std::endl << "For command specific options, type <command> --help."
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
         << "Copyright (C) 2024 Marco Craveiro." << std::endl
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
std::optional<configuration>
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
 * @brief Reads the tracing configuration from the variables map.
*/
std::optional<logging_configuration>
read_logging_configuration(const variables_map& vm) {
    const auto enabled(vm.count(logging_log_enabled_arg) != 0);
    if (!enabled)
        return {};

    logging_configuration r;
    r.filename("ores.console.log");
    r.output_to_console(vm.count(logging_log_to_console_arg) != 0);

    const bool log_dir_set(vm.count(logging_log_dir_arg) != 0);
    if (!log_dir_set) {
        r.output_directory("log");
    }
    else {
        const auto log_dir(vm[logging_log_dir_arg].as<std::string>());
        r.output_directory(log_dir);
    }

    const bool log_level_set(vm.count(logging_log_level_arg) != 0);
    if (!log_level_set) {
        r.severity(logging_log_level_info);
        return r;
    }

    const auto s(vm[logging_log_level_arg].as<std::string>());
    try {
        using ores::utility::log::to_severity_level;
        to_severity_level(s);
        r.severity(s);
    } catch(const std::exception&) {
        BOOST_THROW_EXCEPTION(parser_exception(invalid_log_level + s));
    }
    return r;
}

/**
 * @brief Reads the importing configuration from the variables map.
 */
importing_configuration
read_importing_configuration(const variables_map& vm) {
    importing_configuration r;

    using std::filesystem::absolute;

    bool found_imports(false);
    if (vm.count(importing_curency_config_arg) != 0) {
        found_imports = true;
        const auto ccy_cfgs(vm[importing_curency_config_arg].
            as<std::vector<std::string>>());

        std::vector<std::filesystem::path> currency_configurations;
        currency_configurations.reserve(ccy_cfgs.size());
        for (const auto& ccy_cfg : ccy_cfgs) {
            currency_configurations.push_back(absolute(ccy_cfg));
        }
        r.currency_configurations(currency_configurations);
    }

    if (!found_imports)
        BOOST_THROW_EXCEPTION(parser_exception(missing_import_target));

    return r;
}


/**
 * @brief Contains the processing logic for when the user supplies a command in
 * the command line.
 */
std::optional<configuration>
handle_command(const std::string& command_name, const bool has_help,
    const parsed_options& po, std::ostream& info, variables_map& vm) {

    /*
     * Collect all the unrecognized options from the first pass. It includes the
     * positional command name, so we need to erase it.
     */
    using boost::program_options::include_positional;
    using boost::program_options::collect_unrecognized;
    auto options(collect_unrecognized(po.options, include_positional));
    options.erase(options.begin());

    /*
     * For each command we need to setup their set of options, parse them and
     * then generate the appropriate options.
     */
    configuration r;
    using boost::program_options::command_line_parser;
    if (command_name == importing_command_name) {
        const auto d(make_importing_options_description());
        if (has_help) {
            print_help_command(importing_command_name, d, info);
            return {};
        }

        store(command_line_parser(options).options(d).run(), vm);
        r.importing(read_importing_configuration(vm));
    }

    /*
     * Now process the common options.
     */
    r.logging(read_logging_configuration(vm));
    return r;
}

/**
 * @brief Parses the arguments supplied in the command line and converts them
 * into a configuration object.
 */
std::optional<configuration>
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
        BOOST_THROW_EXCEPTION(parser_exception(invalid_option + "version"));

    /*
     * We can now process the command. Notice that we are supplying the
     * variables map into the handler by reference. This is because we need
     * access to the global options that may have already been setup.
     */
    return handle_command(command_name, has_help, po, info, vm);
}

}

namespace ores::console {

std::optional<configuration>
program_options_parser::parse(const std::vector<std::string>& arguments,
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
