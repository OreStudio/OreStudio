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

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <rfl/json.hpp>
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.workflow.api/messaging/workflow_events.hpp"

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
 * @brief Returns true if the inbound message is part of a workflow execution.
 *
 * A message is a workflow step command if it carries the X-Workflow-Step-Id
 * header. Domain service handlers use this to switch between normal and
 * workflow-driven execution paths.
 */
inline bool is_workflow_command(const ores::nats::message& msg) {
    return msg.headers.contains(std::string(workflow_step_id_header));
}

/**
 * @brief Extracts a workflow header value from an inbound NATS message.
 *
 * Returns empty string if the header is absent.
 */
inline std::string extract_workflow_header(
    const ores::nats::message& msg,
    std::string_view header_name) {

    const auto it = msg.headers.find(std::string(header_name));
    if (it == msg.headers.end()) return {};
    return it->second;
}

/**
 * @brief Context for a workflow step extracted from an inbound NATS message.
 *
 * Captures the step_id, instance_id, and tenant_id headers plus a reference
 * to the NATS client.  Provides complete() and fail() helpers so domain
 * service handlers can publish step-completed events without touching
 * low-level NATS details.
 *
 * Typical usage:
 * @code
 *   auto wf = workflow_step_context::from_message(nats_, msg);
 *   if (!wf) return;           // not a workflow command
 *   try {
 *       auto result = do_work(wf->tenant_id, ...);
 *       wf->complete(rfl::json::write(result));
 *   } catch (const std::exception& e) {
 *       wf->fail(e.what());
 *   }
 * @endcode
 */
struct workflow_step_context {
    std::string step_id;
    std::string instance_id;
    std::string tenant_id;
    ores::nats::service::client* nats;

    /**
     * @brief Extracts the workflow context from a NATS message.
     *
     * Returns std::nullopt if the message is not a workflow step command
     * (i.e. missing the X-Workflow-Step-Id header).
     */
    [[nodiscard]] static std::optional<workflow_step_context>
    from_message(ores::nats::service::client& nats,
        const ores::nats::message& msg) {

        if (!is_workflow_command(msg)) return std::nullopt;
        return workflow_step_context{
            .step_id     = extract_workflow_header(msg, workflow_step_id_header),
            .instance_id = extract_workflow_header(msg, workflow_instance_id_header),
            .tenant_id   = extract_workflow_header(msg, workflow_tenant_id_header),
            .nats        = &nats};
    }

    /**
     * @brief Publishes a successful step-completed event.
     *
     * @param result_json Serialised JSON result payload.
     */
    void complete(const std::string& result_json) const {
        publish(true, result_json, "");
    }

    /**
     * @brief Publishes a failed step-completed event.
     *
     * @param error_msg Human-readable error description.
     */
    void fail(const std::string& error_msg) const {
        publish(false, "", error_msg);
    }

private:
    void publish(bool success, const std::string& result_json,
        const std::string& error_msg) const {

        ores::workflow::messaging::step_completed_event event{
            .workflow_instance_id = instance_id,
            .step_id              = step_id,
            .success              = success,
            .result_json          = result_json,
            .error_message        = error_msg};

        const auto json = rfl::json::write(event);
        const auto data = std::as_bytes(
            std::span{json.data(), json.size()});
        nats->publish(
            ores::workflow::messaging::step_completed_event::nats_subject, data);
    }
};

/**
 * @brief Publishes a step-completed event to the workflow engine.
 *
 * Low-level function for cases where a workflow_step_context is not
 * convenient.  Prefer workflow_step_context::complete() / fail() for
 * new code.
 */
inline void publish_step_completion(
    ores::nats::service::client& nats,
    const std::string& step_id,
    const std::string& instance_id,
    bool success,
    const std::string& result_json,
    const std::string& error_msg) {

    workflow_step_context ctx{
        .step_id = step_id,
        .instance_id = instance_id,
        .tenant_id = {},
        .nats = &nats};

    if (success)
        ctx.complete(result_json);
    else
        ctx.fail(error_msg);
}

}

#endif
