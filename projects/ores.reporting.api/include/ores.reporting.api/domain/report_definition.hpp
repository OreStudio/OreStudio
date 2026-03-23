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
#ifndef ORES_REPORTING_DOMAIN_REPORT_DEFINITION_HPP
#define ORES_REPORTING_DOMAIN_REPORT_DEFINITION_HPP

#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::reporting::domain {

/**
 * @brief Persistent template for a scheduled report.
 *
 * The persistent template for a report. Describes what to run, when to run it,
 * and how to handle concurrent executions. Type-specific configuration (e.g.
 * risk parameters) lives in a separate table keyed by report_definition_id.
 * 
 * Lifecycle is managed through the report_definition_lifecycle FSM machine.
 * fsm_state_id points to the current state in ores_dq_fsm_states_tbl.
 * 
 * scheduler_job_id links to ores_scheduler_job_definitions_tbl.id and is set
 * by the scheduler service when the definition is activated (state: active).
 * It is cleared when the definition is suspended or archived.
 */
struct report_definition final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this report definition.
     */
    boost::uuids::uuid id;

    /**
     * @brief Unique name for the report definition within a tenant.
     */
    std::string name;

    /**
     * @brief Party that owns this report definition.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief Optional human-readable description of the report.
     */
    std::string description;

    /**
     * @brief Report type code (FK to ores_reporting_report_types_tbl).
     */
    std::string report_type;

    /**
     * @brief Current FSM state (FK to ores_dq_fsm_states_tbl). Null until first activation.
     */
    std::optional<boost::uuids::uuid> fsm_state_id;

    /**
     * @brief Validated cron expression driving report recurrence.
     */
    std::string schedule_expression;

    /**
     * @brief Concurrency policy code (FK to ores_reporting_concurrency_policies_tbl).
     */
    std::string concurrency_policy;

    /**
     * @brief Scheduler job UUID (FK to ores_scheduler_job_definitions_tbl). Present only when status is active.
     */
    std::optional<boost::uuids::uuid> scheduler_job_id;

    /**
     * @brief Username of the person who last modified this report definition.
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
