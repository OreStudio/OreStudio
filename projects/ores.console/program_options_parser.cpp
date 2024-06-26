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
#include <string_view>
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
// #include "ores/config.hpp"
#include "ores.utility/log/severity_level.hpp"
#include "ores.utility/log/logging_configuration.hpp"
#include "ores.console/parser_exception.hpp"
#include "ores.console/program_options_parser.hpp"

namespace {

const std::string empty;
const std::string indent("   ");
const std::string run_identifier_prefix("console.");

const std::string more_information("Try --help' for more information.");
const std::string product_version("OreStudio v" /*ORES_VERSION*/); // FIXME
const std::string build_info("FIXME"/*ORES_BUILD_INFO*/);
const std::string usage_error_msg("Usage error: ");
const std::string fatal_error_msg("Fatal Error: " );
const std::string no_command_msg("No command supplied. ");
const std::string code_generation_failure("Code generation failure.");
const std::string log_file_msg("See the log file for details: ");
const std::string errors_msg(" finished with errors.");

const std::string data_command_name("data");
const std::string data_command_desc(
    "Operates directly on data.");

const std::string help_arg("help");
const std::string version_arg("version");
const std::string command_arg("command");
const std::string reference_directory_arg("reference-directory");

const std::string logging_log_enabled_arg("log-enabled");
const std::string logging_log_to_console_arg("log-to-console");
const std::string logging_log_level_arg("log-level");
const std::string logging_log_level_trace("trace");
const std::string logging_log_level_debug("debug");
const std::string logging_log_level_info("info");
const std::string logging_log_level_warn("warn");
const std::string logging_log_level_error("error");

const std::string invalid_log_level("Log level is invalid: ");
const std::string invalid_command("Command is invalid or unsupported: ");
const std::string invalid_option("Option is not valid for command: ");

using boost::program_options::value;
using boost::program_options::variables_map;
using boost::program_options::options_description;
using boost::program_options::positional_options_description;
using ores::console::configuration;
using ores::console::parser_exception;
using ores::utility::log::logging_configuration;

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

    options_description bod("Output");
    bod.add_options()
        ("byproduct-directory", "Directory in which to place all of the "
            "byproducts of the run such as log files, traces, etc.");
    r.add(bod);

    options_description lod("Logging");
    lod.add_options()
        ("log-enabled,e", "Generate a log file.")
        ("log-level,l", value<std::string>(),
            "What level to use for logging. Valid values: trace, debug, info, "
            "warn, error. Defaults to info.")
        ("log-to-console",
            "Output logging to the console, as well as to file.");
    r.add(lod);
    return r;
}

/**
 * @brief Creates the the top-level option descriptions that are
 * hidden to end users.
 */
options_description make_top_level_hidden_options_description() {
    options_description r("Commands");
    r.add_options()
        ("command", value<std::string>(), "Command to execute. "
            "Available commands: generate, convert, dumpspecs.")
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
 * @brief Creates the options related to code generation.
 */
options_description make_generate_options_description() {
    options_description r("Generation");
    r.add_options()
        ("target,t",
            value<std::string>(),
            "Model to generate code for, in any of the supported formats.")
        ("output-directory,o",
            value<std::string>(), "Output directory for the generated code. "
            "Defaults to the current working directory.")
        ("reference-directory,r",
            value<std::vector<std::string>>(), "One or more directories to"
            " check for referenced models.");

    return r;
}

/**
 * @brief ensures the supplied command is a valid command. If not,
 * reports the erros into stream and throws.
 */
void validate_command_name(const std::string& command_name) {
    const bool is_valid_command_name(
        command_name == data_command_name);

    if (is_valid_command_name)
        return;

    BOOST_THROW_EXCEPTION(parser_exception(invalid_command + command_name));
}

/**
 * @brief Prints the header of the help text, applicable to all cases.
 */
void print_help_header(std::ostream& s) {
    s << "Dogen is a Model Driven Engineering tool that processes models"
      << " encoded in supported codecs." << std::endl
      << "Dogen is created by the MASD project. " << std::endl;
}

/**
 * @brief Prints the top-level help text when no command is supplied.
 *
 * @param od top-level options.
 * @param s info stream.
 */
void print_help(const boost::program_options::options_description& od,
    std::ostream& s) {
    print_help_header(s);
    s << "dogen.cli uses a command-based interface: <command> <options>. "
      << std::endl << "See below for a list of valid commands. " << std::endl
      << std::endl << "Global options: " << std::endl << od << std::endl
      <<  "Commands: "<< std::endl << std::endl;

    auto lambda([&](const std::string& name, const std::string& desc) {
                    const unsigned int command_width(15);
                    s << indent << std::setfill(' ') << std::left
                      << std::setw(command_width)
                      << name << desc << std::endl;
                });
    lambda(data_command_name, data_command_desc);

    s << std::endl << "For command specific options, type <command> --help."
      << std::endl;
}

/**
 * @brief Prints help text at the command level.
 *
 * @param command_name name of the command to print help for.
 * @param od command options.
 * @param s info stream.
 */
void print_help_command(const std::string& command_name,
    const boost::program_options::options_description& od, std::ostream& s) {
    print_help_header(s);
    s << "Displaying options specific to the " << command_name << " command. "
      << std::endl
      << "For global options, type --help." << std::endl << std::endl
      << od;
}

/**
 * @brief Print the program's version details.
 *
 * @param s info stream.
 */
void version(std::ostream& s) {
    s << product_version << std::endl
      << "Copyright (C) 2024 Marco Craveiro." << std::endl
      << "License GPLv3: GNU GPL version 3 or later "
      << "<http://gnu.org/licenses/gpl.html>." << std::endl
      << "This is free software: you are free to change and redistribute it."
      << std::endl << "There is NO WARRANTY, to the extent permitted by law."
      << std::endl;

    if (!build_info.empty()) {
        s << build_info << std::endl;
        s << "IMPORTANT: build details are NOT for security purposes."
          << std::endl;
    }
}

/**
 * @brief Contains the processing logic for when the user did not
 * supply a command in the command line.
 */
boost::optional<configuration>
handle_no_command(const bool has_version, const bool has_help,
    const options_description& od, std::ostream& info) {
    /*
     * The only valid options are help or version, so if those are not
     * present we can safely throw.
     */
    if (!has_version && !has_help)
        BOOST_THROW_EXCEPTION(parser_exception(no_command_msg));

    /*
     * Note that we do not mind if the user has supplied both help and
     * version - help takes priority.
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
boost::optional<logging_configuration> read_logging_configuration(
    const std::string& run_identifier, const variables_map& vm,
    const boost::filesystem::path& byproduct_dir) {
    const auto enabled(vm.count(logging_log_enabled_arg) != 0);
    if (!enabled)
        return {};

    logging_configuration r;
    r.filename(run_identifier);
    r.output_to_console(vm.count(logging_log_to_console_arg) != 0);
    r.output_directory(byproduct_dir);

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
 * @brief Contains the processing logic for when the user supplies a
 * command in the command line.
 */
boost::optional<configuration>
handle_command(const std::string& command_name, const bool has_help,
    const boost::program_options::parsed_options& po, std::ostream& info,
    variables_map& vm) {

    /*
     * Collect all the unrecognized options from the first pass. It
     * includes the positional command name, so we need to erase it.
     */
    using boost::program_options::include_positional;
    using boost::program_options::collect_unrecognized;
    auto options(collect_unrecognized(po.options, include_positional));
    options.erase(options.begin());

    /*
     * For each command we need to setup their set of options, parse
     * them and then generate the appropriate options.
     */
    configuration r;
    boost::filesystem::path target;
    using boost::program_options::command_line_parser;
    // typedef boost::optional<configuration> empty_config;
    if (command_name == data_command_name) {
        const auto d(make_generate_options_description());
        if (has_help) {
            print_help_command(data_command_name, d, info);
            return {};
        }

        store(command_line_parser(options).options(d).run(), vm);
        // const auto c(read_data_configuration(vm));
        // target = c.target();
        // r.cli().activity(c);
    }

    /*
     * Now process the common options. We must do this at the end
     * because we require the model name.
     */
    r.logging(read_logging_configuration("FIXME", vm, "FIXME"));
    return r;
}

boost::optional<configuration>
parse(const std::vector<std::string>& arguments, std::ostream& info) {
    /*
     * Create the top-level command line options, parse them and
     * retrieve the results of the parsing. Note that we split then
     * into visible and hidden to avoid showing the hidden options to
     * the user in the help description.
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
    const bool has_command((bool)vm.count(command_arg));
    const bool has_version((bool)vm.count(version_arg));
    const bool has_help((bool)vm.count(help_arg));

    /*
     * First, handle the simpler case where no command is
     * supplied. Note that we only supply the visible options here.
     */
    if (!has_command)
        return handle_no_command(has_version, has_help, visible, info);

    /*
     * If the user supplied a command, we need to retrieve it and
     * ensure it is valid.
     */
    const auto command_name(vm[command_arg].as<std::string>());
    validate_command_name(command_name);

    /*
     * Copying the same approach as git, we also consider version to
     * be invalid at the command level. We don't bother to handle this
     * at program options level, but instead check for the presence of
     * the (supposedly valid, according to program options) version
     * command and throw.
     */
    if (has_version)
        BOOST_THROW_EXCEPTION(parser_exception(invalid_option + "version"));

    /*
     * We can now process the command. Notice that we are suppliying
     * the variables map into the handler by reference. This is
     * because we need access to the global options that may have
     * already been setup.
     */
    return handle_command(command_name, has_help, po, info, vm);
}

}

namespace ores::console {

boost::optional<configuration>
program_options_parser::parse(const std::vector<std::string>& arguments,
    std::ostream& info, std::ostream& err) const {

    try {
        return ::parse(arguments, info);
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
