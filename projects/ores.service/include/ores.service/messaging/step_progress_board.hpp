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
#ifndef ORES_SERVICE_MESSAGING_STEP_PROGRESS_BOARD_HPP
#define ORES_SERVICE_MESSAGING_STEP_PROGRESS_BOARD_HPP

#include "ores.service/export.hpp"
#include <cstdint>
#include <mutex>
#include <optional>
#include <string>
#include <unordered_map>

namespace ores::service::messaging {

/**
 * @brief What a running step last said about its work, in its own units.
 *
 * A phase alone says what the step is doing; a done and a total count say how
 * far through it is.
 */
struct reported_progress final {
    std::string phase;
    std::optional<std::int64_t> done;
    std::optional<std::int64_t> total;
};

/**
 * @brief The latest progress of each step this process runs, by step id.
 *
 * A step's handler reports its progress as it advances, on its own thread, and
 * the renewal of the step's claim, on the delivery's keep-alive, carries the
 * latest report to the engine. The two meet here. The engine fails a step
 * whose progress stands still for its budget, so a step that reports is never
 * failed for being slow, only for stalling.
 *
 * One board serves the process. It is defined in this library rather than in a
 * header, so a handler in one shared library and the renewal in another write
 * and read the same board.
 */
class ORES_SERVICE_EXPORT step_progress_board final {
public:
    /// The process's board.
    static step_progress_board& instance();

    /// Records @p progress as the step's latest, replacing any earlier report.
    void report(const std::string& step_id, reported_progress progress);

    /// The step's latest report, or nothing when it has reported none.
    [[nodiscard]] std::optional<reported_progress> latest(const std::string& step_id) const;

    /// Drops the step's report, once its delivery is settled.
    void forget(const std::string& step_id);

private:
    mutable std::mutex mutex_;
    std::unordered_map<std::string, reported_progress> by_step_;
};

}

#endif
