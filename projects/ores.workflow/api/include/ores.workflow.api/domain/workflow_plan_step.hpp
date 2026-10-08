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
#ifndef ORES_WORKFLOW_API_DOMAIN_WORKFLOW_PLAN_STEP_HPP
#define ORES_WORKFLOW_API_DOMAIN_WORKFLOW_PLAN_STEP_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <cstdint>
#include <string_view>

namespace ores::workflow::domain {

/**
 * @brief One step of the chain a workflow instance was started with.
 *
 * One step of the chain a run was started with. workflow_step records a step
 * that ran and what came back; this records a step that *will* run, from the
 * moment the run starts, so the engine can answer what the chain is before it has
 * dispatched anything.
 *
 * The chain is built per instance by the definition's build_steps, which is a
 * function of the start request and not of the definition alone — tenant
 * provisioning orders one step per kind its request names. A run therefore
 * carries its own chain rather than pointing at one, because the definition that
 * produced it may not produce the same chain twice and must never reshape a run
 * that is already in flight.
 */
struct workflow_plan_step final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID primary key for the plan step.
     */
    boost::uuids::uuid id;

    /**
     * @brief FK reference to the run this chain belongs to.
     */
    boost::uuids::uuid workflow_id;

    /**
     * @brief Zero-based position of this step in the chain the run was given.
     */
    int step_index = 0;

    /**
     * @brief The step's identity, which is the name a consumer reads its result by. Unique within
     * the run, because a consumer cannot say which of two steps with one name it reads.
     */
    std::string name;

    /**
     * @brief The step's name in a person's words, empty when it has no better name than its
     * identity. Carried with the run because a screen shows the chain the run was started with, not
     * the one the definition builds today.
     */
    std::string label;

    /**
     * @brief What the step does, in a person's words.
     */
    std::string description;

    /**
     * @brief NATS subject the step's command is published to.
     */
    std::string command_subject;

    /**
     * @brief NATS subject for the step's compensation, empty when it has none.
     */
    std::string compensation_subject;

    /**
     * @brief How long the step may run before the engine declares it dead. Carried with the run
     * rather than read from the definition, so a definition that shortened a deadline cannot expire
     * a run that was started under a longer one.
     */
    std::int32_t timeout_seconds;

    /**
     * @brief Username of the person who last modified this workflow plan step.
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
    friend bool operator==(const workflow_plan_step&, const workflow_plan_step&) = default;
};

/**
 * @brief Dispatch-key identifier for workflow_plan_step, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const workflow_plan_step&) {
    return "ores.workflow.workflow_plan_step";
}

}

#endif
