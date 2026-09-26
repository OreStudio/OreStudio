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
#pragma once

#include "ores.scheduler.api/domain/job_definition.hpp"
#include "ores.scheduler.core/export.hpp"
#include <chrono>
#include <optional>

namespace ores::scheduler::service {

/**
 * @brief The wall clock is the only input; callers pass it in.
 *
 * The scheduler loop separates these two decisions from the timer, the
 * database and the message bus so that the firing rules can be exercised
 * against fixed instants.
 */

/**
 * @brief True when the job is active and its next occurrence is due.
 *
 * @param last_run The instant the job last ran. When absent, the job is
 *                 treated as last having run one minute before @p now, so a
 *                 freshly loaded schedule can fire on the current tick.
 */
[[nodiscard]] ORES_SCHEDULER_CORE_EXPORT bool
is_due(const domain::job_definition& job,
       std::optional<std::chrono::system_clock::time_point> last_run,
       std::chrono::system_clock::time_point now);

/**
 * @brief The next whole-minute boundary strictly after @p now.
 */
[[nodiscard]] ORES_SCHEDULER_CORE_EXPORT std::chrono::system_clock::time_point
next_minute_boundary(std::chrono::system_clock::time_point now);

} // namespace ores::scheduler::service
