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
#ifndef ORES_WORKFLOW_API_MESSAGING_WORKFLOW_QUERY_PROTOCOL_HPP
#define ORES_WORKFLOW_API_MESSAGING_WORKFLOW_QUERY_PROTOCOL_HPP

#include <string>
#include <vector>
#include <optional>
#include <string_view>

namespace ores::workflow::messaging {

// ============================================================================
// List workflow instances
// ============================================================================

/**
 * @brief Summary of a single workflow instance returned by list_instances.
 *
 * All timestamps are UTC ISO-8601 strings (e.g. "2026-04-10T14:30:00Z").
 * status is a human-readable state name: in_progress, completed, failed,
 * compensating, or compensated.
 */
struct workflow_instance_summary {
    std::string id;
    std::string type;
    std::string status;
    int current_step_index = 0;
    int step_count = 0;
    std::string correlation_id;
    std::string created_by;
    std::string created_at;
    std::optional<std::string> completed_at;
    std::string error;
};

/**
 * @brief Request to list workflow instances for the authenticated tenant.
 *
 * Results are ordered by created_at descending (most recent first).
 * Requires a valid Bearer JWT in the Authorization NATS header.
 */
struct list_workflow_instances_request {
    using response_type = struct list_workflow_instances_response;
    static constexpr std::string_view nats_subject =
        "workflow.v1.instances.list";

    /**
     * @brief Maximum number of instances to return (default 200, max 1000).
     */
    int limit = 200;

    /**
     * @brief Optional status filter. Empty string means return all statuses.
     *
     * Valid values: "in_progress", "completed", "failed",
     *               "compensating", "compensated"
     */
    std::optional<std::string> status_filter;
};

struct list_workflow_instances_response {
    bool success = false;
    std::string message;
    std::vector<workflow_instance_summary> instances;
};

// ============================================================================
// Get workflow steps
// ============================================================================

/**
 * @brief Summary of a single step within a workflow instance.
 */
struct workflow_step_summary {
    std::string id;
    std::string name;
    std::string status;
    int step_index = 0;
    std::string created_at;
    std::optional<std::string> started_at;
    std::optional<std::string> completed_at;
    std::string error;
};

/**
 * @brief Request to retrieve all steps for a specific workflow instance.
 *
 * The instance must belong to the authenticated tenant; the handler returns
 * an error if the instance is owned by a different tenant.
 * Requires a valid Bearer JWT in the Authorization NATS header.
 */
struct get_workflow_steps_request {
    using response_type = struct get_workflow_steps_response;
    static constexpr std::string_view nats_subject =
        "workflow.v1.instances.steps";

    std::string workflow_instance_id;
};

struct get_workflow_steps_response {
    bool success = false;
    std::string message;
    std::vector<workflow_step_summary> steps;
};

}  // namespace ores::workflow::messaging

#endif
