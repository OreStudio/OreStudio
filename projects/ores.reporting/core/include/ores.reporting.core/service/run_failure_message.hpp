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
#ifndef ORES_REPORTING_CORE_SERVICE_RUN_FAILURE_MESSAGE_HPP
#define ORES_REPORTING_CORE_SERVICE_RUN_FAILURE_MESSAGE_HPP

#include "ores.reporting.core/export.hpp"
#include <cstddef>
#include <string>

namespace ores::reporting::service {

/**
 * @brief How many problems of each severity a run holds.
 *
 * The count is of what a reader finds in the run's Errors, which is not the
 * count of the rows the node marshalled: a line the run keeps nothing for is
 * not a problem, and the same failure stated twice is one row.
 */
struct run_problems {
    std::size_t errors = 0;
    std::size_t warnings = 0;

    /// Whether the run holds no problem at all.
    [[nodiscard]] bool empty() const {
        return errors == 0 && warnings == 0;
    }
};

/**
 * @brief What a failed run states about itself, in one sentence.
 *
 * A run whose engine log gave it rows does not repeat the transcript: it states
 * how many problems the log produced and where they are. The rows are the
 * account of what went wrong, one per engine failure, and a reader follows them
 * rather than reading the engine's output again.
 *
 * A run whose log gave nothing to parse keeps the engine's own reason whole,
 * because then that reason is the only account anyone has of the failure.
 *
 * @param outcome The compute outcome the run failed with.
 * @param engine_reason The reason the node stated for the failed result.
 * @param recorded The problems the run now holds.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::string
run_failure_message(int outcome, const std::string& engine_reason, const run_problems& recorded);

}

#endif
