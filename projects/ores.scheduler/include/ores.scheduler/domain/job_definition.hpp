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
#ifndef ORES_SCHEDULER_DOMAIN_JOB_DEFINITION_HPP
#define ORES_SCHEDULER_DOMAIN_JOB_DEFINITION_HPP

#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.scheduler/domain/cron_expression.hpp"

namespace ores::scheduler::domain {

/**
 * @brief Persistent plan for a pg_cron scheduled SQL job.
 *
 * Metadata overlay for a pg_cron cron.job entry. Tracks the job name,
 * cron expression, SQL command, target database, and active state.
 */
struct job_definition final {
    /**
     * @brief UUID primary key for the job definition.
     */
    boost::uuids::uuid id;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Party that owns this job definition.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief pg_cron job ID linking to cron.job.jobid.
     *
     * Null when the job has not yet been scheduled or has been paused.
     */
    std::optional<std::int64_t> cron_job_id;

    /**
     * @brief Unique name passed to pg_cron.
     */
    std::string job_name;

    /**
     * @brief Human-readable label for UI.
     */
    std::string description;

    /**
     * @brief SQL command to execute.
     */
    std::string command;

    /**
     * @brief Validated cron expression.
     */
    cron_expression schedule_expression;

    /**
     * @brief Target PostgreSQL database name.
     */
    std::string database_name;

    /**
     * @brief False = paused (unscheduled from pg_cron).
     */
    bool is_active = false;

    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Username of the person who last modified this job definition.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
