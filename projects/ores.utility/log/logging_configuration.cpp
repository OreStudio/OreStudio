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
#include <boost/throw_exception.hpp>
#include "ores.utility/log/severity_level.hpp"
#include "ores.utility/log/logging_exception.hpp"
#include "ores.utility/log/logging_configuration.hpp"

namespace ores::utility::log {

namespace {

const std::string logging_log_enabled_arg("log-enabled");
const std::string logging_log_to_console_arg("log-to-console");
const std::string logging_log_level_arg("log-level");
const std::string logging_log_dir_arg("log-directory");
const std::string logging_log_filename_arg("log-filename");
const std::string logging_log_level_info("info");

}

boost::program_options::options_description
logging_configuration::make_options_description(bool include_filename_option) {
    using boost::program_options::value;
    using boost::program_options::options_description;

    options_description r("Logging");
    r.add_options()
        ("log-enabled,e", "Generate a log file.")
        ("log-level,l", value<std::string>(),
            "What level to use for logging. Valid values: trace, debug, info, "
            "warn, error. Defaults to info.")
        ("log-to-console",
            "Output logging to the console, as well as to file.")
        ("log-directory", value<std::string>(),
            "Where to place the log files.");

    if (include_filename_option) {
        r.add_options()
            ("log-filename", value<std::string>(),
                "Name of the log file.");
    }

    return r;
}

std::optional<logging_options>
logging_configuration::read_options(
    const boost::program_options::variables_map& vm,
    const std::string& default_filename,
    const std::string& default_directory) {

    const bool enabled(vm.count(logging_log_enabled_arg) != 0);
    if (!enabled)
        return {};

    logging_options r;

    // Set filename
    if (vm.count(logging_log_filename_arg) != 0) {
        r.filename = vm[logging_log_filename_arg].as<std::string>();
    } else {
        r.filename = default_filename;
    }

    // Set console output
    r.output_to_console = vm.count(logging_log_to_console_arg) != 0;

    // Set directory
    if (vm.count(logging_log_dir_arg) != 0) {
        r.output_directory = vm[logging_log_dir_arg].as<std::string>();
    } else {
        r.output_directory = default_directory;
    }

    // Set log level
    if (vm.count(logging_log_level_arg) != 0) {
        const auto s(vm[logging_log_level_arg].as<std::string>());
        try {
            to_severity_level(s);
            r.severity = s;
        } catch(const std::exception&) {
            BOOST_THROW_EXCEPTION(logging_exception(
                    std::format("Log level is invalid: {}!", s)));
        }
    } else {
        r.severity = logging_log_level_info;
    }

    return r;
}

}
