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
#ifndef ORES_WORKFLOW_DOMAIN_WORKFLOW_INSTANCE_HPP
#define ORES_WORKFLOW_DOMAIN_WORKFLOW_INSTANCE_HPP

#include <chrono>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>

namespace ores::workflow::domain {

/**
 * @brief A single execution of a named workflow.
 *
 * Tracks the lifecycle of a workflow execution, including its type, status,
 * the serialised request that triggered it, and any result or error produced.
 * Instances are append-mostly; status transitions are the primary mutation.
 */
struct workflow_instance final {
    /**
     * @brief UUID primary key for the workflow instance.
     */
    boost::uuids::uuid id;

    /**
     * @brief Tenant that owns this workflow instance.
     */
    boost::uuids::uuid tenant_id;

    /**
     * @brief Workflow type name, e.g. 'provision_parties_workflow'.
     */
    std::string type;

    /**
     * @brief FK to the FSM state record (ores_dq_fsm_states_tbl) for this instance.
     */
    boost::uuids::uuid state_id;

    /**
     * @brief Serialised JSON payload of the originating request.
     */
    std::string request_json;

    /**
     * @brief Serialised JSON result, populated on successful completion.
     */
    std::string result_json;

    /**
     * @brief Human-readable error message, populated on failure.
     */
    std::string error;

    /**
     * @brief Distributed tracing correlation ID (Nats-Correlation-Id header value).
     */
    std::string correlation_id;

    /**
     * @brief Username or service identity that initiated the workflow.
     */
    std::string created_by;

    /**
     * @brief Timestamp when the workflow reached a terminal state.
     */
    std::optional<std::chrono::system_clock::time_point> completed_at;

    /**
     * @brief Timestamp when this record was created.
     */
    std::chrono::system_clock::time_point created_at;
};

}

#endif
