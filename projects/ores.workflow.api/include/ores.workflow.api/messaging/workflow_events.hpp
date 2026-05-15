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
#ifndef ORES_WORKFLOW_API_MESSAGING_WORKFLOW_EVENTS_HPP
#define ORES_WORKFLOW_API_MESSAGING_WORKFLOW_EVENTS_HPP

#include <cstdint>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>
#include <rfl.hpp>
#include "ores.workflow.api/messaging/step_log_types.hpp"

namespace ores::workflow::messaging {

/**
 * @brief Terminal outcome of a workflow step.
 *
 * Replaces the previous bool success field to express three distinct states:
 *   completed               — succeeded with no issues
 *   completed_with_warnings — ran to completion but some items failed
 *   failed                  — fatal; saga compensation will be triggered
 *
 * Serialises as a lowercase string for the same DB queryability reasons
 * as step_log_level.
 */
enum class step_outcome : std::uint8_t {
    completed               = 0,
    completed_with_warnings = 1,
    failed                  = 2
};

[[nodiscard]] inline std::string_view to_string(step_outcome v) {
    switch (v) {
    case step_outcome::completed:               return "completed";
    case step_outcome::completed_with_warnings: return "completed_with_warnings";
    case step_outcome::failed:                  return "failed";
    }
    throw std::invalid_argument("Out-of-range step_outcome");
}

[[nodiscard]] inline step_outcome step_outcome_from_string(std::string_view sv) {
    if (sv == "completed")               return step_outcome::completed;
    if (sv == "completed_with_warnings") return step_outcome::completed_with_warnings;
    if (sv == "failed")                  return step_outcome::failed;
    throw std::invalid_argument("Invalid step_outcome: '" + std::string(sv) + "'");
}

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
     * @brief Terminal outcome of this step.
     */
    step_outcome outcome = step_outcome::completed;

    /**
     * @brief Serialised JSON result payload from the domain service.
     *
     * Stored in workflow_step.response_json and passed as input to
     * subsequent step command builders.
     */
    std::string result_json;

    /**
     * @brief Human-readable error message on fatal failure.
     *
     * Stored in workflow_step.error and workflow_instance.error.
     * Non-empty only when outcome == failed.
     */
    std::string error_message;

    /**
     * @brief Ordered list of log entries emitted by this step.
     *
     * Serialised to step_log_json in the workflow step record.  Empty for
     * steps that produce no user-visible diagnostic output.
     */
    std::vector<step_log_entry> log;
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

namespace rfl {

template<>
struct Reflector<ores::workflow::messaging::step_log_level> {
    using ReflType = std::string;

    static ores::workflow::messaging::step_log_level to(const ReflType& s) {
        return ores::workflow::messaging::step_log_level_from_string(s);
    }

    static ReflType from(const ores::workflow::messaging::step_log_level& v) {
        return std::string(ores::workflow::messaging::to_string(v));
    }
};

template<>
struct Reflector<ores::workflow::messaging::step_outcome> {
    using ReflType = std::string;

    static ores::workflow::messaging::step_outcome to(const ReflType& s) {
        return ores::workflow::messaging::step_outcome_from_string(s);
    }

    static ReflType from(const ores::workflow::messaging::step_outcome& v) {
        return std::string(ores::workflow::messaging::to_string(v));
    }
};

}

#endif
