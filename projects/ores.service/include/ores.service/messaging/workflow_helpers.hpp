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
#ifndef ORES_SERVICE_MESSAGING_WORKFLOW_HELPERS_HPP
#define ORES_SERVICE_MESSAGING_WORKFLOW_HELPERS_HPP

#include <span>
#include <string>
#include <string_view>
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"

namespace ores::service::messaging {

/**
 * @brief NATS header name for the workflow step idempotency key.
 *
 * Domain services extract this from inbound commands to detect re-dispatched
 * commands (idempotency check) and echo it back in the step-completed event.
 */
inline constexpr std::string_view workflow_step_id_header =
    "X-Workflow-Step-Id";

/**
 * @brief NATS header name for the parent workflow instance.
 */
inline constexpr std::string_view workflow_instance_id_header =
    "X-Workflow-Instance-Id";

/**
 * @brief NATS header name for the tenant that owns the workflow instance.
 *
 * Set by the workflow engine on all step-command publishes. Domain service
 * handlers use this to build the correct tenant-scoped database context when
 * processing a workflow step command (i.e. when is_workflow_command() is true).
 */
inline constexpr std::string_view workflow_tenant_id_header = "X-Tenant-Id";

/**
 * @brief Publishes a step-completed event to the workflow engine.
 *
 * Domain service handlers that participate in a workflow call this after
 * processing a workflow step command. The workflow engine receives this event
 * and advances or compensates the workflow accordingly.
 *
 * The caller must echo back the @p step_id and @p instance_id values received
 * in the X-Workflow-Step-Id and X-Workflow-Instance-Id headers of the command.
 *
 * @param nats        Raw NATS client (fire-and-forget publish).
 * @param step_id     Step UUID echoed from X-Workflow-Step-Id header.
 * @param instance_id Workflow instance UUID echoed from X-Workflow-Instance-Id.
 * @param success     true if the step operation succeeded, false otherwise.
 * @param result_json Serialised JSON result payload (empty string on failure).
 * @param error_msg   Human-readable error description (empty string on success).
 */
inline void publish_step_completion(
    ores::nats::service::client& nats,
    const std::string& step_id,
    const std::string& instance_id,
    bool success,
    const std::string& result_json,
    const std::string& error_msg) {

    // Build the event JSON manually to avoid a dependency on ores.workflow.
    // The step_completed_event struct is defined in ores.workflow/messaging/
    // workflow_events.hpp; we replicate the field names here for independence.
    std::string json = "{";
    json += "\"workflow_instance_id\":\"" + instance_id + "\",";
    json += "\"step_id\":\"" + step_id + "\",";
    json += "\"success\":" + std::string(success ? "true" : "false") + ",";
    json += "\"result_json\":" + (result_json.empty() ? "\"\"" : result_json) + ",";
    json += "\"error_message\":\"" + error_msg + "\"";
    json += "}";

    const auto data = std::as_bytes(std::span{json.data(), json.size()});
    nats.publish("workflow.v1.events.step-completed", data);
}

/**
 * @brief Extracts a workflow header value from an inbound NATS message.
 *
 * Returns empty string if the header is absent. Domain service handlers use
 * this to obtain the X-Workflow-Step-Id and X-Workflow-Instance-Id values
 * for idempotency checking and step-completion publishing.
 *
 * @param msg         Inbound NATS message.
 * @param header_name One of workflow_step_id_header or workflow_instance_id_header.
 */
inline std::string extract_workflow_header(
    const ores::nats::message& msg,
    std::string_view header_name) {

    const auto it = msg.headers.find(std::string(header_name));
    if (it == msg.headers.end()) return {};
    return it->second;
}

/**
 * @brief Returns true if the inbound message is part of a workflow execution.
 *
 * A message is a workflow step command if it carries the X-Workflow-Step-Id
 * header. Domain service handlers use this to switch between normal and
 * workflow-driven execution paths.
 */
inline bool is_workflow_command(const ores::nats::message& msg) {
    return msg.headers.contains(std::string(workflow_step_id_header));
}

}

#endif
