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
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.scheduler.api/domain/cron_expression.hpp"

namespace ores::scheduler::domain {

/**
 * @brief Persistent plan for an in-process scheduled job.
 *
 * Represents a single scheduled task managed by the OreStudio in-process
 * scheduler. Tracks the job name, cron expression, SQL command or MQ message
 * payload, and active state.
 */
struct job_definition final {
    /**
     * @brief UUID primary key for the job definition.
     */
    boost::uuids::uuid id;

    /**
     * @brief Tenant identifier for multi-tenancy isolation. Null for system jobs.
     */
    std::optional<boost::uuids::uuid> tenant_id;

    /**
     * @brief Party that owns this job definition. Null for system jobs.
     */
    std::optional<boost::uuids::uuid> party_id;

    /**
     * @brief Unique name for this job.
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
     * @brief Type of action to execute on each firing.
     * "execute_sql"  — run the SQL in the command field.
     * "nats_publish" — publish a NATS message; action_payload carries subject and body.
     */
    std::string action_type = "execute_sql";

    /**
     * @brief JSON payload for the action.
     * For nats_publish: {"subject":"<nats_subject>","report_definition_id":"<uuid>","tenant_id":"<uuid>"}
     */
    std::string action_payload = "{}";

    /**
     * @brief False = paused (not scheduled).
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
