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
#include <format>
#include <ostream>
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include <magic_enum/magic_enum.hpp>
#include "ores.cli/config/format.hpp"
#include "ores.utility/version/version.hpp"
#include "ores.utility/log/severity_level.hpp"
#include "ores.utility/database/database_configuration.hpp"
#include "ores.cli/config/entity.hpp"
#include "ores.cli/config/parser_exception.hpp"
#include "ores.cli/config/parser.hpp"

namespace {

const std::string indent("   ");
const std::string more_information("Try --help' for more information.");
const std::string product_version("OreStudio v" ORES_VERSION);
const std::string build_info(ORES_BUILD_INFO);
const std::string usage_error_msg("Usage error: ");
const std::string no_command_msg("No command supplied. ");

const std::string entity_arg("entity");
const std::string import_command_name("import");
const std::string import_command_desc("Imports data into the system.");
const std::string import_targets_arg("target");

const std::string export_command_name("export");
const std::string export_command_desc("Exports data from the system.");
const std::string export_as_of_arg("as-of");
const std::string export_key_arg("key");
const std::string export_all_versions_arg("all-versions");
const std::string export_format_arg("format");

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

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::parsed_options;
using boost::program_options::options_description;
using boost::program_options::positional_options_description;

using ores::utility::log::logging_options;
using ores::cli::config::entity;
using ores::cli::config::format;
using ores::cli::config::options;
using ores::cli::config::import_options;
using ores::cli::config::export_options;
using ores::cli::config::parser_exception;

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
        command_name == import_command_name ||
        command_name == export_command_name);

    if (!is_valid_command_name)
    {
        BOOST_THROW_EXCEPTION(parser_exception(
                std::format("Invalid or unsupported command: {}",
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

    lambda(import_command_name, import_command_desc);
    lambda(export_command_name, export_command_desc);

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
 * @brief Reads the logging configuration from the variables map.
*/
std::optional<logging_options>
read_logging_configuration(const variables_map& vm) {
    const auto enabled(vm.count(logging_log_enabled_arg) != 0);
    if (!enabled)
        return {};

    logging_options r;
    r.filename = "ores.cli.log";
    r.output_to_console = vm.count(logging_log_to_console_arg) != 0;

    const bool log_dir_set(vm.count(logging_log_dir_arg) != 0);
    if (!log_dir_set) {
        r.output_directory = "log";
    }
    else {
        const auto log_dir(vm[logging_log_dir_arg].as<std::string>());
        r.output_directory = log_dir;
    }

    const bool log_level_set(vm.count(logging_log_level_arg) != 0);
    if (!log_level_set) {
        r.severity = logging_log_level_info;
        return r;
    }

    const auto s(vm[logging_log_level_arg].as<std::string>());
    try {
        using ores::utility::log::to_severity_level;
        to_severity_level(s);
        r.severity = s;
    } catch(const std::exception&) {
        BOOST_THROW_EXCEPTION(parser_exception(
                std::format("Log level is invalid: {}!", s)));
    }
    return r;
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
    const auto name_mapper(database_configuration::make_environment_mapper());

    if (command_name == import_command_name) {
        auto d(make_import_options_description());
        d.add(db_desc).add(logging_desc);
        if (has_help) {
            print_help_command(import_command_name, d, info);
            return {};
        }

        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        r.importing = read_import_options(vm);
    } else if (command_name == export_command_name) {
        auto d(make_export_options_description());
        d.add(db_desc).add(logging_desc);
        if (has_help) {
            print_help_command(export_command_name, d, info);
            return {};
        }

        store(command_line_parser(o).options(d).run(), vm);
        store(parse_environment(d, name_mapper), vm);
        r.exporting = read_export_options(vm);
    }

    r.database = database_configuration::read_options(vm);

    /*
     * Now process the common options.
     */
    r.logging = read_logging_configuration(vm);
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
