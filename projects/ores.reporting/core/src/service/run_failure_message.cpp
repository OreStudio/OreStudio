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
#include "ores.reporting.core/service/run_failure_message.hpp"
#include <format>
#include <string_view>

namespace ores::reporting::service {
namespace {

/**
 * @brief The one line the node led its reason with.
 *
 * The node states its own verdict first and appends the engine's output under
 * it. The output is what the run's rows already say, row by row, so only the
 * verdict is repeated.
 */
std::string first_statement_of(const std::string& reason) {
    const auto line_end = reason.find('\n');
    return line_end == std::string::npos ? reason : reason.substr(0, line_end);
}

/// A count in the words a reader counts in: @c 1 @c error, @c 24 @c errors.
std::string counted(std::size_t count, std::string_view singular) {
    return std::format("{} {}{}", count, singular, count == 1 ? "" : "s");
}

/// The problems the run holds, named by severity.
std::string problems_stated(const run_problems& recorded) {
    if (recorded.errors > 0 && recorded.warnings > 0)
        return std::format("{} and {}",
                           counted(recorded.errors, "error"),
                           counted(recorded.warnings, "warning"));
    if (recorded.errors > 0)
        return counted(recorded.errors, "error");
    return counted(recorded.warnings, "warning");
}

}

std::string run_failure_message(int outcome,
                                const std::string& engine_reason,
                                const run_problems& recorded) {
    if (recorded.empty()) {
        if (engine_reason.empty())
            return std::format("The run's compute job failed with outcome {}.", outcome);
        return std::format("The run's compute job failed with outcome {}: {}", outcome, engine_reason);
    }

    const auto stated = first_statement_of(engine_reason);
    return std::format("The run's compute job failed with outcome {}{}. Its log records {}; "
                       "the rows are in Errors.",
                       outcome,
                       stated.empty() ? std::string() : std::format(": {}", stated),
                       problems_stated(recorded));
}

}
