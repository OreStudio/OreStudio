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

namespace ores::scheduler::service {

/**
 * @brief Coordinator between OreStudio job definitions and pg_cron.
 *
 * Translates C++ job_definition objects into pg_cron SQL calls and keeps the
 * ores_scheduler_job_definitions_tbl in sync with cron.job.
 *
 * Lifecycle:
 *   1. Registration — schedule() calls cron.schedule() then persists the
 *      returned cron_job_id into our table.
 *   2. Execution    — pg_cron fires independently; no C++ involvement.
 *   3. Observation  — get_all_definitions() / get_job_history() query our
 *      table and cron.job_run_details respectively.
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
     * @brief Register a job with pg_cron and persist the definition.
     *
     * Executes: SELECT cron.schedule(job_name, schedule_expression, command)
     * Stores the returned bigint as cron_job_id in our table.
     * If a job with this name already exists in pg_cron, pg_cron updates it
     * in-place and returns the existing jobid.
     */
    domain::job_definition schedule(domain::job_definition def,
                                    const std::string& change_reason_code,
                                    const std::string& change_commentary);

    /**
     * @brief Remove a job from pg_cron and mark it as inactive.
     *
     * Executes: SELECT cron.unschedule(job_name)
     * Sets cron_job_id to NULL in our table (definition is retained for
     * history; set is_active = false).
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
};

} // namespace ores::scheduler::service
