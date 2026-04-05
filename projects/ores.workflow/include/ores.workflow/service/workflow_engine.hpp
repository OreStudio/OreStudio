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
#ifndef ORES_WORKFLOW_SERVICE_WORKFLOW_ENGINE_HPP
#define ORES_WORKFLOW_SERVICE_WORKFLOW_ENGINE_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.workflow/domain/workflow_instance.hpp"
#include "ores.workflow/repository/workflow_instance_repository.hpp"
#include "ores.workflow/repository/workflow_step_repository.hpp"
#include "ores.workflow/service/fsm_state_map.hpp"
#include "ores.workflow/service/workflow_registry.hpp"

namespace ores::workflow::service {

/**
 * @brief Persistent event-driven workflow engine.
 *
 * Processes step-completed events from domain services and advances or
 * compensates the corresponding workflow instance. All state is persisted
 * to PostgreSQL before and after each NATS publish so that the engine can
 * restart at any time and resume all in-flight workflows.
 *
 * Thread safety: instances of this class are not thread-safe. Each NATS
 * message callback runs on the NATS IO thread; a single engine instance
 * is shared across all queue-group subscribers, protected by the NATS
 * library's serialisation guarantees.
 */
class workflow_engine {
private:
    inline static std::string_view logger_name =
        "ores.workflow.service.workflow_engine";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs the engine.
     *
     * @param nats            Raw NATS client used for fire-and-forget publishes.
     * @param ctx             Service-account database context (no tenant filter).
     * @param registry        Registry of all known workflow definitions.
     * @param instance_states Pre-loaded FSM state map for workflow_instance.
     * @param step_states     Pre-loaded FSM state map for workflow_step.
     */
    workflow_engine(ores::nats::service::client& nats,
        ores::database::context ctx,
        const workflow_registry& registry,
        fsm_state_map instance_states,
        fsm_state_map step_states);

    /**
     * @brief Handles a step-completed event from a domain service.
     *
     * Fire-and-forget: no reply is sent. Advances the workflow to the next
     * step, or begins compensation if the step failed.
     */
    void on_step_completed(ores::nats::message msg);

    /**
     * @brief Handles a start-workflow message.
     *
     * Creates a new workflow_instance record, persists and dispatches step 0.
     * Fire-and-forget: no reply is sent.
     */
    void on_start_workflow(ores::nats::message msg);

    /**
     * @brief Recovers all in-progress workflows on service startup.
     *
     * Queries for workflow_instance rows with state=in_progress, loads the
     * current step for each, and re-dispatches the step command using the
     * same step_id (idempotency key). Domain services deduplicate via the
     * step_id and re-publish their completion events.
     */
    void recover_in_progress();

private:
    /**
     * @brief Publishes a step command to the domain service.
     *
     * Includes X-Workflow-Instance-Id, X-Workflow-Step-Id, and X-Tenant-Id
     * NATS headers so domain services can extract the idempotency key and
     * build the correct tenant-scoped database context.
     */
    void publish_command(const domain::workflow_step& step,
        const boost::uuids::uuid& instance_id,
        const boost::uuids::uuid& tenant_id);

    /**
     * @brief Dispatches the next step in the workflow after a success.
     *
     * Builds the next step's command, persists the step record, publishes
     * the command, and advances the instance's current_step_index.
     */
    void dispatch_next_step(domain::workflow_instance& instance,
        const std::string& last_result_json);

    /**
     * @brief Begins saga compensation for a failed workflow instance.
     *
     * Iterates completed steps in reverse order, builds and publishes
     * compensation commands, and transitions the instance to compensating.
     */
    void begin_compensation(const domain::workflow_instance& instance,
        const std::string& failure_msg);

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    const workflow_registry& registry_;
    fsm_state_map instance_states_;
    fsm_state_map step_states_;
    repository::workflow_instance_repository instance_repo_;
    repository::workflow_step_repository step_repo_;
};

}

#endif
