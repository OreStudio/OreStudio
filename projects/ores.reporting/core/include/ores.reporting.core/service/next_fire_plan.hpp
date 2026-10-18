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
#ifndef ORES_REPORTING_CORE_SERVICE_NEXT_FIRE_PLAN_HPP
#define ORES_REPORTING_CORE_SERVICE_NEXT_FIRE_PLAN_HPP

#include "ores.reporting.api/domain/report_definition.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.reporting.core/export.hpp"
#include "ores.scheduler.api/messaging/scheduling_operations_protocol.hpp"
#include <vector>

namespace ores::reporting::service {

/**
 * @brief Joins the definitions a screen drew to the next fire the scheduler states.
 *
 * The two halves come from different services and neither can answer alone:
 * the definition names the scheduler job it fires from, and the scheduler states
 * when that job next fires. The join is stated here rather than in a client so
 * that the screen reads one row per definition and no client decides what an
 * absent time means.
 *
 * One entry per definition, in the order the definitions were given. A
 * definition that holds no scheduler job has an entry with no time, because it
 * is run on demand; a definition whose job states no next fire, because it is
 * paused or its expression cannot be advanced, has an entry with no time too.
 *
 * @param definitions The definitions the caller may read, as the store answered.
 * @param jobs The scheduler's status, one entry per job it holds.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::vector<messaging::definition_next_fire>
plan_next_fire(const std::vector<domain::report_definition>& definitions,
               const std::vector<ores::scheduler::messaging::job_schedule_status>& jobs);

}

#endif
