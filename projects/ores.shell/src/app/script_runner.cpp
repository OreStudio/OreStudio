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
#include <cctype>
#include <cstdlib>
#include <istream>
#include <ostream>
#include <string>

namespace ores::shell::app {

namespace {

// Expand =$VAR= and =${VAR}= references from the process environment so
// a library script can pull the current environment (NATS URL, subject
// prefix, TLS paths, ...) out of the =.env=-derived environment instead
// of hard-coding it. A lone =$=, or a =$= not followed by a valid name,
// is left as a literal. On the first reference to an undefined variable
// the name is reported via @p missing and expansion stops being trusted
// (the caller turns this into a failed line).
std::string expand_env_vars(const std::string& in, std::string& missing) {
    std::string out;
    out.reserve(in.size());
    for (std::size_t i = 0; i < in.size();) {
        if (in[i] != '$') {
            out += in[i++];
            continue;
        }
        std::size_t j = i + 1;
        const bool braced = (j < in.size() && in[j] == '{');
        if (braced)
            ++j;
        const std::size_t name_start = j;
        while (j < in.size() &&
               (std::isalnum(static_cast<unsigned char>(in[j])) || in[j] == '_'))
            ++j;
        const std::string name = in.substr(name_start, j - name_start);
        if (name.empty()) {
            // A lone '$' or '${' with no name: keep the text verbatim.
            out += in.substr(i, j - i);
            i = j;
            continue;
        }
        if (braced) {
            if (j >= in.size() || in[j] != '}') {
                // Unterminated '${...': keep it literal rather than guess.
                out += in.substr(i, j - i);
                i = j;
                continue;
            }
            ++j; // consume '}'
        }
        if (const char* val = std::getenv(name.c_str())) {
            out += val;
        } else if (missing.empty()) {
            missing = name;
        }
        i = j;
    }
    return out;
}

} // anonymous namespace

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

        // Strip leading/trailing whitespace. The search sets match, so
        // once start hits a non-whitespace character end cannot be npos
        // and trimmed cannot be empty.
        auto start = line.find_first_not_of(" \t\r");
        if (start == std::string::npos)
            continue;
        auto end = line.find_last_not_of(" \t\r");
        auto trimmed = line.substr(start, end - start + 1);
        if (trimmed[0] == '#')
            continue;

        // Expand environment references before the line is run, echoing
        // the expanded form so the operator sees exactly what executed.
        std::string missing;
        const auto expanded = expand_env_vars(trimmed, missing);
        if (!missing.empty()) {
            out << "> " << trimmed << std::endl;
            out << "✗ Undefined environment variable: $" << missing
                << std::endl;
            if (!continue_on_error) {
                r.aborted = true;
                r.aborted_line = line_num;
                r.aborted_command = trimmed;
                return r;
            }
            continue;
        }

        out << "> " << expanded << std::endl;
        command_feedback::reset();
        feed(expanded);
        ++r.executed;

        if (command_feedback::failed() && !continue_on_error) {
            r.aborted = true;
            r.aborted_line = line_num;
            r.aborted_command = expanded;
            return r;
        }
    }
    return r;
}

}
