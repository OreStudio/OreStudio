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
#ifndef ORES_ORE_CORE_LOG_ENGINE_RUN_VERDICT_HPP
#define ORES_ORE_CORE_LOG_ENGINE_RUN_VERDICT_HPP

#include "ores.ore.core/export.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::log {

/**
 * @brief What an engine run did, judged from what it left behind.
 *
 * @see judge_engine_run
 */
struct engine_run_outcome {
    /**
     * @brief Whether the run produced analytics.
     */
    bool succeeded = false;

    /**
     * @brief Why the run failed, with the engine's own errors; empty when it
     * did not.
     */
    std::string failure;
};

/**
 * @brief Judges an engine run from the files it wrote and the log it left.
 *
 * The engine's exit code is not a verdict. ORE is a Java process that logs its
 * own errors and returns 0, so a run that priced nothing looks exactly like a
 * run that priced everything to whoever reads the code alone. What the run left
 * behind is the verdict: a run that wrote no analytics failed, and the engine's
 * own =ERROR= lines say why.
 *
 * The log is *not* evidence that anything was computed. The engine writes it
 * beside its results, so a directory holding only the log is a run that
 * produced nothing.
 *
 * @param output_file_names  the file names in the engine's output directory.
 * @param engine_log         the engine's own log, as it wrote it.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT engine_run_outcome
judge_engine_run(const std::vector<std::string>& output_file_names, std::string_view engine_log);

}

#endif
