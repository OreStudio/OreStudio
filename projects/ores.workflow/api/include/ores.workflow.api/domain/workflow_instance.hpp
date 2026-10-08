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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_WORKFLOW_API_DOMAIN_WORKFLOW_INSTANCE_HPP
#define ORES_WORKFLOW_API_DOMAIN_WORKFLOW_INSTANCE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::workflow::domain {

/**
 * @brief A single execution of a named workflow.
 *
 * Tracks the lifecycle of a workflow execution, including its type, the entity it
 * acts on, its status, the serialised request that triggered it, and any result or
 * error produced. Instances are append-mostly; status transitions are the primary
 * mutation.
 */
struct workflow_instance final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID primary key for the workflow instance.
     */
    boost::uuids::uuid id;

    /**
     * @brief Workflow type name, e.g. 'provision_tenant_workflow'.
     */
    std::string type;

    /**
     * @brief What the run acts on, named by the caller: 'tenant' for a run that provisions a
     * tenant. The engine stores it and never interprets it, so a component states its own kind
     * without the engine learning that component's vocabulary.
     *
     * A run that acts on no entity leaves it empty.
     */
    std::string target_kind;

    /**
     * @brief Identity of the entity the run acts on, in the kind target_kind states. It is not a
     * foreign key: the engine cannot hold one to every component's tables, so it is a soft
     * reference, the way a status row names its badge.
     *
     * The target is not the owner. tenant_id states the tenant whose work the run is, which scopes
     * the run and governs who reads it. The target states what the run works on, which may be a
     * different tenant entirely. Provisioning is the case that makes the difference plain: the run
     * belongs to the tenant that asked for it, so that tenant can follow its own work, and it acts
     * on the tenant it creates, which cannot read the run because its session does not exist yet.
     */
    boost::uuids::uuid target_id;

    /**
     * @brief FK to the FSM state record (ores_workflow_fsm_states_tbl) for this instance.
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
     * @brief Zero-based index of the step currently being executed.
     */
    int current_step_index = 0;

    /**
     * @brief Total number of steps in this workflow definition.
     */
    int step_count = 0;

    /**
     * @brief Timestamp when the workflow reached a terminal state.
     */
    std::optional<std::chrono::system_clock::time_point> completed_at;

    /**
     * @brief Timestamp of the most recent step-completed event processed.
     */
    std::optional<std::chrono::system_clock::time_point> last_event_at;

    /**
     * @brief Username of the person who last modified this workflow instance.
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
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const workflow_instance&, const workflow_instance&) = default;
};

/**
 * @brief Dispatch-key identifier for workflow_instance, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const workflow_instance&) {
    return "ores.workflow.workflow_instance";
}

}

#endif
