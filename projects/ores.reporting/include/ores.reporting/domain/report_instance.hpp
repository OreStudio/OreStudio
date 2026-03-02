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
#ifndef ORES_REPORTING_DOMAIN_REPORT_INSTANCE_HPP
#define ORES_REPORTING_DOMAIN_REPORT_INSTANCE_HPP

#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::reporting::domain {

/**
 * @brief A single execution of a report definition.
 *
 * A single execution of a report_definition. Created automatically when the
 * scheduler fires a trigger for an active definition.
 * 
 * Lifecycle is managed through the report_instance_lifecycle FSM machine.
 * fsm_state_id points to the current state in ores_dq_fsm_states_tbl.
 * 
 * started_at is NULL when the instance is cancelled or skipped before execution
 * begins. completed_at is NULL while running or in a terminal-before-start state.
 */
struct report_instance final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this report instance.
     */
    boost::uuids::uuid id;

    /**
     * @brief Party that owns this report instance (inherited from the definition).
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The report definition that produced this instance.
     */
    boost::uuids::uuid definition_id;

    /**
     * @brief Current FSM state (FK to ores_dq_fsm_states_tbl).
     */
    std::optional<boost::uuids::uuid> fsm_state_id;

    /**
     * @brief Scheduler trigger run ID that caused this instance to be created.
     */
    std::int64_t trigger_run_id;

    /**
     * @brief Execution log or error message.
     */
    std::string output_message;

    /**
     * @brief When execution began. NULL if cancelled before starting.
     */
    std::optional<std::chrono::system_clock::time_point> started_at;

    /**
     * @brief When execution completed. NULL while running or before start.
     */
    std::optional<std::chrono::system_clock::time_point> completed_at;

    /**
     * @brief Username of the person who last modified this report instance.
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
