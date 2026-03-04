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

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.database/domain/context.hpp"
#include "ores.scheduler/domain/job_definition.hpp"
#include "ores.scheduler/domain/job_instance.hpp"
#include "ores.scheduler/repository/job_definition_repository.hpp"
#include "ores.scheduler/repository/job_instance_repository.hpp"

namespace ores::scheduler::service {

/**
 * @brief Coordinator for OreStudio in-process job scheduling.
 *
 * Provides CRUD operations for job definitions and access to job execution
 * history. The actual scheduling is performed by scheduler_loop; this class
 * handles the management plane only.
 *
 * Lifecycle:
 *   1. Registration — schedule() persists the job_definition with is_active=true.
 *   2. Execution    — scheduler_loop fires jobs independently.
 *   3. Observation  — get_all_definitions() / get_job_history() query our tables.
 *
 * Thread safety: not thread-safe. Each request should create its own instance
 * (or use a per-request database context).
 */
class cron_scheduler final {
public:
    using context = ores::database::context;

    explicit cron_scheduler(context ctx);

    // -------------------------------------------------------------------------
    // Registration phase
    // -------------------------------------------------------------------------

    /**
     * @brief Persist a job definition and mark it as active.
     *
     * Writes the job_definition to ores_scheduler_job_definitions_tbl with
     * is_active=true. The in-process scheduler_loop will pick it up on the
     * next reload.
     */
    domain::job_definition schedule(domain::job_definition def,
                                    const std::string& change_reason_code,
                                    const std::string& change_commentary);

    /**
     * @brief Mark a job as inactive (paused).
     *
     * Sets is_active=false on the current active record. The scheduler_loop
     * will skip the job after the next reload.
     */
    void unschedule(const boost::uuids::uuid& job_definition_id,
                    const std::string& modified_by,
                    const std::string& change_reason_code,
                    const std::string& change_commentary);

    // -------------------------------------------------------------------------
    // Observation phase
    // -------------------------------------------------------------------------

    /**
     * @brief All job definitions visible to the current tenant+party context.
     */
    [[nodiscard]] std::vector<domain::job_definition> get_all_definitions();

    /**
     * @brief Execution history for a specific job, newest-first.
     *
     * @param limit Maximum number of instances returned (default: 100).
     */
    [[nodiscard]] std::vector<domain::job_instance>
    get_job_history(const boost::uuids::uuid& job_definition_id,
                    std::size_t limit = 100);

private:
    context ctx_;
    repository::job_definition_repository repo_;
    repository::job_instance_repository inst_repo_;
};

} // namespace ores::scheduler::service
