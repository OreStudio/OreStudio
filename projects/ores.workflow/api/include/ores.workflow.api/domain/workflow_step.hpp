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
#ifndef ORES_WORKFLOW_API_DOMAIN_WORKFLOW_STEP_HPP
#define ORES_WORKFLOW_API_DOMAIN_WORKFLOW_STEP_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::workflow::domain {

/**
 * @brief A single step within a workflow instance.
 *
 * Records the execution of one step in a saga workflow, including the step
 * index, name, status, request/response payloads, and timing. Steps reference
 * their parent workflow instance via workflow_id.
 */
struct workflow_step final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID primary key for the workflow step.
     */
    boost::uuids::uuid id;

    /**
     * @brief FK reference to the parent workflow instance.
     */
    boost::uuids::uuid workflow_id;

    /**
     * @brief Zero-based ordinal position of this step within the workflow. Negative values denote
     * compensation steps.
     */
    int step_index = 0;

    /**
     * @brief Human-readable step name, e.g. 'save_party'.
     */
    std::string name;

    /**
     * @brief FK to the FSM state record (ores_workflow_fsm_states_tbl) for this step.
     */
    boost::uuids::uuid state_id;

    /**
     * @brief Serialised JSON payload sent to the downstream service for this step.
     */
    std::string request_json;

    /**
     * @brief Serialised JSON response received from the downstream service.
     */
    std::string response_json;

    /**
     * @brief Human-readable error message if the step failed.
     */
    std::string error;

    /**
     * @brief NATS subject to which the step command was published.
     */
    std::string command_subject;

    /**
     * @brief Serialised JSON command payload published to the domain service.
     */
    std::string command_json;

    /**
     * @brief Timestamp when the step command was published to NATS.
     */
    std::optional<std::chrono::system_clock::time_point> command_published_at;

    /**
     * @brief Idempotency key (echoes the step UUID) sent as X-Workflow-Step-Id header.
     */
    std::string idempotency_key;

    /**
     * @brief NATS subject for the compensation command. Empty if this step has no compensation.
     */
    std::string compensation_subject;

    /**
     * @brief Serialised JSON compensation command payload, populated when compensation is
     * triggered.
     */
    std::string compensation_json;

    /**
     * @brief Timestamp when the step began executing.
     */
    std::optional<std::chrono::system_clock::time_point> started_at;

    /**
     * @brief Timestamp when the step reached a terminal state.
     */
    std::optional<std::chrono::system_clock::time_point> completed_at;

    /**
     * @brief Username of the person who last modified this workflow step.
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
    friend bool operator==(const workflow_step&, const workflow_step&) = default;
};

/**
 * @brief Dispatch-key identifier for workflow_step, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const workflow_step&) {
    return "ores.workflow.workflow_step";
}

}

#endif
