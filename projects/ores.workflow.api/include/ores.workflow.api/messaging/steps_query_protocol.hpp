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
#ifndef ORES_WORKFLOW_API_MESSAGING_STEPS_QUERY_PROTOCOL_HPP
#define ORES_WORKFLOW_API_MESSAGING_STEPS_QUERY_PROTOCOL_HPP

#include <string>
#include <string_view>

namespace ores::workflow::messaging {

// ============================================================================
// Get step result (idempotency check for domain service handlers)
// ============================================================================

/**
 * @brief Queries the workflow engine for a previously-completed step result.
 *
 * Domain service handlers send this before executing a workflow step command.
 * If the step already completed (e.g. the command was re-dispatched after a
 * restart), the handler can replay the cached result without re-executing.
 *
 * No authentication required — the step ID is treated as an opaque
 * idempotency key; the response carries no tenant-sensitive data.
 */
struct get_step_result_request {
    using response_type = struct get_step_result_response;
    static constexpr std::string_view nats_subject =
        "workflow.v1.steps.get-result";

    /**
     * @brief UUID of the workflow step to look up, echoed from X-Workflow-Step-Id.
     */
    std::string step_id;
};

struct get_step_result_response {
    /**
     * @brief true if the step exists and has reached a terminal state.
     *
     * false if the step is not found, is pending, or is still in_progress.
     * Callers should proceed with normal execution when found == false.
     */
    bool found = false;

    /**
     * @brief true if the step completed successfully; false if it failed.
     *
     * Valid only when found == true.
     */
    bool success = false;

    /**
     * @brief Serialised JSON result from the original execution.
     *
     * Non-empty when found == true && success == true.
     */
    std::string result_json;

    /**
     * @brief Human-readable error from the original execution.
     *
     * Non-empty when found == true && success == false.
     */
    std::string error_message;
};

}  // namespace ores::workflow::messaging

#endif
