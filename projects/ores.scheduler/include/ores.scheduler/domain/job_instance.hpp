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

#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.scheduler/domain/job_status.hpp"

namespace ores::scheduler::domain {

/**
 * @brief A read-only record of a single pg_cron job execution.
 *
 * Populated by querying cron.job_run_details joined with
 * ores_scheduler_job_definitions_tbl. Tenant and party isolation is enforced
 * at the application layer: only run details for cron_job_ids that belong to
 * the current tenant+party context are ever returned.
 */
struct job_instance final {
    std::int64_t instance_id;                                 ///< cron.job_run_details.runid
    std::int64_t cron_job_id;                                 ///< cron.job_run_details.jobid
    boost::uuids::uuid parent_job_id;                         ///< Our job_definition.id
    job_status status = job_status::starting;
    std::string return_message;                               ///< stdout or error text
    std::chrono::system_clock::time_point start_time;
    std::optional<std::chrono::system_clock::time_point> end_time;

    /**
     * @brief Wall-clock duration of the execution, if it has completed.
     */
    [[nodiscard]] std::optional<std::chrono::seconds> duration() const noexcept {
        if (!end_time)
            return std::nullopt;
        return std::chrono::duration_cast<std::chrono::seconds>(*end_time - start_time);
    }
};

} // namespace ores::scheduler::domain
