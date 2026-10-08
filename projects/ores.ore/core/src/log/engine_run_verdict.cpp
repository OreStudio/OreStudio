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
#include "ores.ore.core/log/engine_run_verdict.hpp"
#include "ores.ore.core/log/ore_log_parser.hpp"
#include <algorithm>
#include <cstddef>
#include <format>

namespace ores::ore::log {

namespace {

/** The engine's log, which sits beside its results rather than among them. */
constexpr std::string_view engine_log_name = "log.txt";

/**
 * @brief How many of the engine's errors a failure names.
 *
 * A run can log the same error once per trade. The message has to stay
 * readable, so it carries the last few and says how many it left out.
 */
constexpr std::size_t max_reported_errors = 5;

bool is_analytics(const std::string& file_name) {
    return !file_name.empty() && file_name != engine_log_name;
}

/** The messages of the engine's ERROR lines, in the order it wrote them. */
std::vector<std::string> error_messages(std::string_view engine_log, std::size_t& total_lines) {
    std::vector<std::string> errors;
    total_lines = 0;

    std::size_t first = 0;
    while (first < engine_log.size()) {
        const auto end = engine_log.find('\n', first);
        const auto line = engine_log.substr(
            first, end == std::string_view::npos ? std::string_view::npos : end - first);
        ++total_lines;

        if (const auto parsed = parse_ore_log_line(line); parsed && parsed->level == "error")
            errors.push_back(parsed->message);

        if (end == std::string_view::npos)
            break;
        first = end + 1;
    }

    return errors;
}

}

engine_run_outcome judge_engine_run(const std::vector<std::string>& output_file_names,
                                    std::string_view engine_log) {
    engine_run_outcome outcome;

    // A run that wrote analytics did the job, whatever it also logged. Failing
    // it here would turn a working run into a failed one, which is the opposite
    // of what this check is for.
    if (std::ranges::any_of(output_file_names, is_analytics)) {
        outcome.succeeded = true;
        return outcome;
    }

    std::size_t total_lines = 0;
    const auto errors = error_messages(engine_log, total_lines);

    outcome.failure = "The engine produced no analytics.";
    if (errors.empty()) {
        if (!engine_log.empty())
            outcome.failure += std::format(" It logged {} lines and no error.", total_lines);
        return outcome;
    }

    const auto omitted = errors.size() > max_reported_errors ? errors.size() - max_reported_errors :
                                                               static_cast<std::size_t>(0);
    outcome.failure += omitted == 0 ?
                           " The engine reported:" :
                           std::format(" The engine reported {} errors; the last {} are:",
                                       errors.size(),
                                       max_reported_errors);
    for (auto it = errors.begin() + static_cast<std::ptrdiff_t>(omitted); it != errors.end(); ++it)
        outcome.failure += "\n  " + *it;

    return outcome;
}

}
