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
#include "ores.scheduler.core/service/schedule_decision.hpp"

namespace ores::scheduler::service {

bool is_due(const domain::job_definition& job,
            std::optional<std::chrono::system_clock::time_point> last_run,
            std::chrono::system_clock::time_point now) {
    if (!job.is_active)
        return false;

    const auto after = last_run.value_or(now - std::chrono::minutes(1));
    return job.schedule_expression.next_occurrence(after) <= now;
}

std::chrono::system_clock::time_point
next_minute_boundary(std::chrono::system_clock::time_point now) {
    const auto now_t = std::chrono::system_clock::to_time_t(now);
    const auto next_t = ((now_t / 60) + 1) * 60;
    return std::chrono::system_clock::from_time_t(next_t);
}

} // namespace ores::scheduler::service
