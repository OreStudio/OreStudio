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
#include "ores.comms.shell/app/commands/script_commands.hpp"

#include <fstream>
#include <ostream>
#include <cli/cli.h>
#include "ores.logging/make_logger.hpp"

namespace ores::comms::shell::app::commands {

using namespace ores::logging;

namespace {

inline std::string_view anon_logger_name =
    "ores.comms.shell.app.commands.script_commands";

auto& anon_lg() {
    static auto instance = make_logger(anon_logger_name);
    return instance;
}

}

void script_commands::register_commands(cli::Menu& root,
    cli::CliSession*& active_session) {
    root.Insert("load",
        [&active_session](std::ostream& out, std::string filename) {
            if (!active_session) {
                out << "Error: no active session." << std::endl;
                return;
            }

            std::ifstream file(filename);
            if (!file.is_open()) {
                out << "Error: cannot open file: " << filename << std::endl;
                BOOST_LOG_SEV(anon_lg(), error) << "Cannot open script file: "
                                                << filename;
                return;
            }

            out << "Loading script: " << filename << std::endl;
            BOOST_LOG_SEV(anon_lg(), info) << "Loading script: " << filename;

            int line_num = 0;
            int executed = 0;
            std::string line;
            while (std::getline(file, line)) {
                ++line_num;

                // Skip empty lines and comments
                if (line.empty())
                    continue;
                if (line[0] == '#')
                    continue;

                // Strip leading/trailing whitespace
                auto start = line.find_first_not_of(" \t");
                if (start == std::string::npos)
                    continue;
                auto end = line.find_last_not_of(" \t\r");
                auto trimmed = line.substr(start, end - start + 1);

                out << "> " << trimmed << std::endl;
                active_session->Feed(trimmed);
                ++executed;
            }

            out << "Script complete: " << executed << " command"
                << (executed != 1 ? "s" : "") << " executed." << std::endl;
            BOOST_LOG_SEV(anon_lg(), info) << "Script complete: " << executed
                                           << " commands executed from "
                                           << filename;
        },
        "Load and execute a .ores script file",
        {"filename"});
}

}
