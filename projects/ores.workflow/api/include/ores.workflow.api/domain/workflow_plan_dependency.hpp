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
#ifndef ORES_WORKFLOW_API_DOMAIN_WORKFLOW_PLAN_DEPENDENCY_HPP
#define ORES_WORKFLOW_API_DOMAIN_WORKFLOW_PLAN_DEPENDENCY_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string_view>

namespace ores::workflow::domain {

/**
 * @brief One edge of a run's chain: a step and a step that reads its result.
 *
 * One edge of a run's chain. The producer names the step that writes a result and
 * the consumer the step that reads it, both by their position in the same run's
 * chain.
 *
 * This is a relation and not a list on the step, because a step may read any
 * number of producers and a producer may feed any number of consumers. It is
 * what makes the chain a graph the engine can read: the questions "what is
 * waiting", "what may run now" and "what has to be rolled back, in what order"
 * are all queries over these rows and the steps' states, where they used to be a
 * parse of a document and a cursor.
 */
struct workflow_plan_dependency final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID primary key for the dependency.
     */
    boost::uuids::uuid id;

    /**
     * @brief FK reference to the run this edge belongs to.
     */
    boost::uuids::uuid workflow_id;

    /**
     * @brief Position of the step that reads the result.
     */
    int consumer_step_index = 0;

    /**
     * @brief Position of the step that writes it. Always before the consumer's position, because
     * the engine advances along the chain in the order it was declared.
     */
    int producer_step_index = 0;

    /**
     * @brief Username of the person who last modified this workflow plan dependency.
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
    friend bool operator==(const workflow_plan_dependency&,
                           const workflow_plan_dependency&) = default;
};

/**
 * @brief Dispatch-key identifier for workflow_plan_dependency, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const workflow_plan_dependency&) {
    return "ores.workflow.workflow_plan_dependency";
}

}

#endif
