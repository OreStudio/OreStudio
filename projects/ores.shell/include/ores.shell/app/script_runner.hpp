/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_SHELL_APP_SCRIPT_RUNNER_HPP
#define ORES_SHELL_APP_SCRIPT_RUNNER_HPP

#include <functional>
#include <iosfwd>
#include <string>

namespace ores::shell::app {

/**
 * @brief Outcome of executing a .ores script.
 */
struct script_result {
    /// Number of commands fed to the session.
    int executed = 0;
    /// True when the script stopped at a failed command.
    bool aborted = false;
    /// 1-based line number of the failed command (when aborted).
    int aborted_line = 0;
    /// The command that failed (when aborted).
    std::string aborted_command;
};

/**
 * @brief Execute a .ores script: one command per line, '#' comments
 * and blank lines skipped, surrounding whitespace trimmed.
 *
 * Each command is echoed to out with a "> " prefix and handed to
 * feed. Failure is observed through command_feedback: the flag is
 * reset before each command and inspected afterwards. By default the
 * first failed command aborts the run; continue_on_error restores
 * the keep-going behaviour. The runner is separated from the load
 * command so these semantics are unit-testable without a cli session.
 */
script_result run_script(std::istream& in,
                         const std::function<void(const std::string&)>& feed,
                         std::ostream& out,
                         bool continue_on_error);

}

#endif
