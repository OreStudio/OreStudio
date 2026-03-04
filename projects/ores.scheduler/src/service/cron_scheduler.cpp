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
    : ctx_(std::move(ctx)) {}

domain::job_definition
cron_scheduler::schedule(domain::job_definition def,
                         const std::string& /*change_reason_code*/,
                         const std::string& /*change_commentary*/) {
    BOOST_LOG_SEV(lg(), info) << "Scheduling job: " << def.job_name;

    def.is_active = true;
    repo_.write(ctx_, def);

    BOOST_LOG_SEV(lg(), info) << "Job definition persisted: " << def.job_name;
    return def;
}

void cron_scheduler::unschedule(const boost::uuids::uuid& job_definition_id,
                                const std::string& /*modified_by*/,
                                const std::string& /*change_reason_code*/,
                                const std::string& /*change_commentary*/) {
    BOOST_LOG_SEV(lg(), info) << "Unscheduling job: " << job_definition_id;

    const auto def = repo_.find_by_id(ctx_, job_definition_id);
    if (!def) {
        BOOST_LOG_SEV(lg(), warn) << "Job not found: " << job_definition_id;
        throw std::runtime_error("Job definition not found: "
                                 + boost::uuids::to_string(job_definition_id));
    }

    // Mark is_active = 0 on the current active record without creating a new
    // bitemporal version.
    const auto id_str = boost::uuids::to_string(job_definition_id);

    const std::string sql =
        "UPDATE ores_scheduler_job_definitions_tbl "
        "SET is_active = 0 "
        "WHERE id = $1::uuid "
        "AND valid_to = ores_utility_infinity_timestamp_fn()";

    execute_parameterized_command(ctx_, sql, {id_str},
        lg(), "Marking job definition as inactive.");

    BOOST_LOG_SEV(lg(), info) << "Job marked inactive: " << job_definition_id;
}

std::vector<domain::job_definition>
cron_scheduler::get_all_definitions() {
    return repo_.read_latest(ctx_);
}

std::vector<domain::job_instance>
cron_scheduler::get_job_history(const boost::uuids::uuid& job_definition_id,
                                std::size_t limit) {
    return inst_repo_.read_latest(ctx_, job_definition_id, limit);
}

} // namespace ores::scheduler::service
