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
#include "ores.qt/CommandLineParser.hpp"

#include <boost/process/v2/pid.hpp>

namespace ores::qt {

CommandLineParser::CommandLineParser() {
    setupOptions();
}

void CommandLineParser::setupOptions() {
    parser_.setApplicationDescription("Qt UI for ORE Studio");
    parser_.addHelpOption();
    parser_.addVersionOption();

    // Logging options
    parser_.addOption({
        {"e", "log-enabled"},
        "Generate a log file."
    });

    parser_.addOption({
        {"l", "log-level"},
        "What level to use for logging. Valid values: trace, debug, info, "
        "warn, error.",
        "level",
        "info"
    });

    parser_.addOption({
        "log-to-console",
        "Output logging to the console, as well as to file."
    });

    parser_.addOption({
        "log-directory",
        "Where to place the log files.",
        "directory",
        "log"
    });

    parser_.addOption({
        "log-filename",
        "Name of the log file.",
        "filename",
        "ores.qt.log"
    });

    parser_.addOption({
        "log-include-pid",
        "Include the process ID in the log filename."
    });
}

void CommandLineParser::process(const QCoreApplication& app) {
    parser_.process(app);
}

bool CommandLineParser::isLoggingEnabled() const {
    return parser_.isSet("log-enabled");
}

utility::log::logging_options CommandLineParser::loggingOptions() const {
    utility::log::logging_options r;

    // If logging is not enabled, return empty options (logging disabled)
    if (!isLoggingEnabled()) {
        return r;
    }

    // Build the filename
    std::string filename = parser_.value("log-filename").toStdString();
    if (parser_.isSet("log-include-pid")) {
        // Insert PID before the extension
        auto dot_pos = filename.rfind('.');
        if (dot_pos != std::string::npos) {
            filename = filename.substr(0, dot_pos) + "." +
                std::to_string(boost::process::v2::current_pid()) +
                filename.substr(dot_pos);
        } else {
            filename += "." + std::to_string(boost::process::v2::current_pid());
        }
    }

    r.filename = filename;
    r.output_to_console = parser_.isSet("log-to-console");
    r.output_directory = parser_.value("log-directory").toStdString();
    r.severity = parser_.value("log-level").toStdString();

    return r;
}

}
