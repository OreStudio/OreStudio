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
#include "ores.shell/app/script_runner.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include <istream>
#include <ostream>

namespace ores::shell::app {

script_result run_script(std::istream& in,
                         const std::function<void(const std::string&)>& feed,
                         std::ostream& out,
                         bool continue_on_error) {
    script_result r;
    int line_num = 0;
    std::string line;
    while (std::getline(in, line)) {
        ++line_num;

        // Skip empty lines and comments.
        if (line.empty())
            continue;
        if (line[0] == '#')
            continue;

        // Strip leading/trailing whitespace.
        auto start = line.find_first_not_of(" \t");
        if (start == std::string::npos)
            continue;
        auto end = line.find_last_not_of(" \t\r");
        auto trimmed = line.substr(start, end - start + 1);
        if (trimmed[0] == '#')
            continue;

        out << "> " << trimmed << std::endl;
        command_feedback::reset();
        feed(trimmed);
        ++r.executed;

        if (command_feedback::failed() && !continue_on_error) {
            r.aborted = true;
            r.aborted_line = line_num;
            r.aborted_command = trimmed;
            return r;
        }
    }
    return r;
}

}
