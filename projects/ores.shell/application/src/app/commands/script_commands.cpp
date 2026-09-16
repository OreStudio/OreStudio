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
#include "ores.shell/app/commands/script_commands.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.shell/app/command_args.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include "ores.shell/app/script_runner.hpp"
#include <cli/cli.h>
#include <fstream>
#include <ostream>

namespace ores::shell::app::commands {

using namespace ores::logging;

namespace {

inline std::string_view anon_logger_name = "ores.shell.app.commands.script_commands";

auto& anon_lg() {
    static auto instance = make_logger(anon_logger_name);
    return instance;
}

}

void script_commands::register_commands(cli::Menu& root, cli::CliSession*& active_session) {
    root.Insert(
        "load",
        [&active_session](std::ostream& out, std::vector<std::string> args) {
            auto parsed = parse_args(args, {{.name = "continue-on-error"}});
            if (!parsed) {
                fail(out) << parsed.error() << std::endl;
                return;
            }
            if (parsed->positionals.size() != 1) {
                fail(out) << "Usage: load <filename> [--continue-on-error]" << std::endl;
                return;
            }
            const auto& filename = parsed->positionals.front();
            const bool continue_on_error = parsed->flag_set("continue-on-error");

            if (!active_session) {
                fail(out) << "Error: no active session." << std::endl;
                return;
            }

            std::ifstream file(filename);
            if (!file.is_open()) {
                fail(out) << "Error: cannot open file: " << filename << std::endl;
                BOOST_LOG_SEV(anon_lg(), error) << "Cannot open script file: " << filename;
                return;
            }

            out << "Loading script: " << filename << std::endl;
            BOOST_LOG_SEV(anon_lg(), info) << "Loading script: " << filename;

            auto result = run_script(
                file,
                [&active_session](const std::string& command) { active_session->Feed(command); },
                out,
                continue_on_error);

            if (result.aborted) {
                // fail() keeps the flag set so an enclosing load
                // (this command runs under run_script when scripts
                // nest) aborts too.
                fail(out) << "Script aborted at line " << result.aborted_line << ": "
                          << result.aborted_command << std::endl;
                BOOST_LOG_SEV(anon_lg(), error)
                    << "Script aborted at line " << result.aborted_line << " of " << filename
                    << ": " << result.aborted_command;
                return;
            }

            out << "Script complete: " << result.executed << " command"
                << (result.executed != 1 ? "s" : "") << " executed." << std::endl;
            BOOST_LOG_SEV(anon_lg(), info)
                << "Script complete: " << result.executed << " commands executed from " << filename;
        },
        "Load and execute a .ores script file (aborts on first error; "
        "--continue-on-error to keep going)",
        {"filename [--continue-on-error]"});
}

}
