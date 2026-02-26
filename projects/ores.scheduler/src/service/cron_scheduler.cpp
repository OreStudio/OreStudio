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
#include "ores.scheduler/service/cron_scheduler.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::scheduler::service {

using namespace ores::logging;
using namespace ores::database::repository;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.scheduler.service.cron_scheduler");
    return instance;
}

} // anonymous namespace

cron_scheduler::cron_scheduler(context ctx)
    : ctx_(std::move(ctx)), repo_(ctx_) {}

domain::job_definition
cron_scheduler::schedule(domain::job_definition def,
                         const std::string& change_reason_code,
                         const std::string& change_commentary) {
    BOOST_LOG_SEV(lg(), info) << "Scheduling job: " << def.job_name;

    // Persist the definition first (sets our UUID, version etc.)
    repo_.save(def, change_reason_code, change_commentary);

    // Call pg_cron via schedule_in_database so that the background worker
    // (which lives in the postgres database by default) schedules the job
    // to execute in the current application database. This allows multiple
    // OreStudio environments to share a single PostgreSQL cluster while each
    // keeping their jobs isolated to their own database.
    const std::string sql =
        "SELECT cron.schedule_in_database($1, $2, $3, current_database())";
    const auto rows = execute_parameterized_string_query(
        ctx_, sql,
        {def.job_name,
         def.schedule_expression.to_string(),
         def.command},
        lg(), "calling cron.schedule_in_database");

    if (rows.empty()) {
        BOOST_LOG_SEV(lg(), error) << "cron.schedule returned no rows";
        throw std::runtime_error("cron.schedule() returned no result");
    }

    const std::int64_t cron_job_id = std::stoll(rows.front());
    BOOST_LOG_SEV(lg(), debug) << "pg_cron assigned jobid: " << cron_job_id;

    // Link pg_cron's jobid back to our definition.
    repo_.update_cron_job_id(def.id, cron_job_id, def.modified_by);
    def.cron_job_id = cron_job_id;

    return def;
}

void cron_scheduler::unschedule(const boost::uuids::uuid& job_definition_id,
                                const std::string& modified_by,
                                const std::string& change_reason_code,
                                const std::string& change_commentary) {
    BOOST_LOG_SEV(lg(), info) << "Unscheduling job: " << job_definition_id;

    const auto def = repo_.find_by_id(job_definition_id);
    if (!def) {
        BOOST_LOG_SEV(lg(), warn) << "Job not found: " << job_definition_id;
        throw std::runtime_error("Job definition not found: "
                                 + boost::uuids::to_string(job_definition_id));
    }

    // Remove from pg_cron by name. cron.unschedule() resolves the job name
    // from the shared cron.job catalog in the postgres database.
    const std::string sql = "SELECT cron.unschedule($1)";
    execute_parameterized_string_query(ctx_, sql, {def->job_name},
        lg(), "calling cron.unschedule");


    // Mark definition as inactive (clears cron_job_id, sets is_active=false).
    repo_.clear_cron_job_id(job_definition_id, modified_by);
}

std::vector<domain::job_definition>
cron_scheduler::get_all_definitions() const {
    return repo_.get_all();
}

std::vector<domain::job_instance>
cron_scheduler::get_job_history(const boost::uuids::uuid& job_definition_id,
                                std::size_t limit) const {
    return repo_.get_job_history(job_definition_id, limit);
}

} // namespace ores::scheduler::service
