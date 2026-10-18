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
#include "ores.reporting.core/service/next_fire_plan.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <map>
#include <string>
#include <utility>

namespace ores::reporting::service {

namespace {

/// The next fire each job states, by the job's id as the scheduler states it.
std::map<std::string, std::optional<std::string>>
next_fire_by_job(const std::vector<ores::scheduler::messaging::job_schedule_status>& jobs) {
    std::map<std::string, std::optional<std::string>> times;
    for (const auto& job : jobs)
        times.insert_or_assign(job.job_definition_id, job.next_fire_at);
    return times;
}

}

std::vector<messaging::definition_next_fire>
plan_next_fire(const std::vector<domain::report_definition>& definitions,
               const std::vector<ores::scheduler::messaging::job_schedule_status>& jobs) {
    const auto times = next_fire_by_job(jobs);

    std::vector<messaging::definition_next_fire> rows;
    rows.reserve(definitions.size());
    for (const auto& definition : definitions) {
        messaging::definition_next_fire row;
        row.definition_id = definition.id;
        if (definition.scheduler_job_id) {
            const auto it = times.find(boost::uuids::to_string(*definition.scheduler_job_id));
            if (it != times.end())
                row.next_fire_at = it->second;
        }
        rows.push_back(std::move(row));
    }
    return rows;
}

}
