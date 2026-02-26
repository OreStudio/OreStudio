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

#include <cstdint>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.scheduler/domain/cron_expression.hpp"

namespace ores::scheduler::domain {

/**
 * @brief The persistent plan for a scheduled SQL job.
 *
 * Corresponds to a row in ores_scheduler_job_definitions_tbl, which is the
 * OreStudio metadata overlay for a pg_cron cron.job entry.
 *
 * The cron_job_id is assigned by pg_cron after cron.schedule() is called and
 * links this definition to cron.job.jobid. It is absent when the job has not
 * yet been scheduled or has been paused (unscheduled from pg_cron while the
 * definition is retained in our table).
 */
struct job_definition final {
    boost::uuids::uuid id;                        ///< Our UUID primary key.
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system(); ///< Tenant isolation.
    boost::uuids::uuid party_id;                  ///< Party isolation.
    std::optional<std::int64_t> cron_job_id;      ///< FK to cron.job.jobid.
    std::string job_name;                         ///< Unique name passed to pg_cron.
    std::string description;                      ///< Human-readable label for UI.
    std::string command;                          ///< SQL to execute.
    cron_expression schedule_expression;          ///< Validated cron expression.
    std::string database_name;                    ///< Target PostgreSQL database.
    bool is_active = true;                        ///< False = paused (unscheduled).
    int version = 0;                              ///< Optimistic concurrency version.
    std::string modified_by;                      ///< Audit: who last modified.
};

} // namespace ores::scheduler::domain
