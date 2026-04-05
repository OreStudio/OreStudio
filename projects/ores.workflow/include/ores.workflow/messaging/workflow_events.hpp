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
#ifndef ORES_WORKFLOW_MESSAGING_WORKFLOW_EVENTS_HPP
#define ORES_WORKFLOW_MESSAGING_WORKFLOW_EVENTS_HPP

#include <string>
#include <string_view>

namespace ores::workflow::messaging {

/**
 * @brief Fire-and-forget event published by domain services on step completion.
 *
 * Published to workflow.v1.events.step-completed by any domain service that
 * participates in a workflow. The workflow engine subscribes to this subject
 * (queue-group) and advances or compensates the workflow accordingly.
 *
 * The step_id must echo the X-Workflow-Step-Id header from the command that
 * triggered this step. It is used as the idempotency key: the engine checks
 * that the referenced workflow_step is still in_progress before acting.
 */
struct step_completed_event {
    static constexpr std::string_view nats_subject =
        "workflow.v1.events.step-completed";

    /**
     * @brief UUID of the parent workflow instance.
     */
    std::string workflow_instance_id;

    /**
     * @brief UUID of the workflow step being completed.
     *
     * Echoed from the X-Workflow-Step-Id header of the originating command.
     */
    std::string step_id;

    /**
     * @brief true if the step succeeded; false if it failed.
     */
    bool success = false;

    /**
     * @brief Serialised JSON result payload from the domain service.
     *
     * Stored in workflow_step.response_json and passed as input to
     * subsequent step command builders.
     */
    std::string result_json;

    /**
     * @brief Human-readable error message on failure.
     *
     * Stored in workflow_step.error and workflow_instance.error.
     */
    std::string error_message;
};

/**
 * @brief Fire-and-forget message to start a new workflow instance.
 *
 * Published by client services (e.g. ores.reporting.service) to request
 * that the workflow engine create and drive a new workflow_instance.
 */
struct start_workflow_message {
    static constexpr std::string_view nats_subject = "workflow.v1.start";

    /**
     * @brief Workflow type name to look up in the registry.
     */
    std::string type;

    /**
     * @brief Tenant the workflow runs on behalf of.
     */
    std::string tenant_id;

    /**
     * @brief Serialised JSON payload for the initial step's command builder.
     */
    std::string request_json;

    /**
     * @brief Optional distributed tracing correlation ID.
     */
    std::string correlation_id;

    /**
     * @brief Optional pre-generated workflow instance UUID.
     *
     * When non-empty the engine uses this UUID for the new workflow_instance
     * record instead of generating one. Callers that need to return the
     * instance ID before the engine has processed the message (e.g. the
     * ore_import handler) pre-generate a UUID here so they can include it
     * in the synchronous response to the client.
     *
     * Empty string (default) means the engine generates a fresh UUID.
     */
    std::string instance_id;
};

}

#endif
