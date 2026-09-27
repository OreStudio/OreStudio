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
#ifndef ORES_ANALYTICS_API_DOMAIN_CREDIT_SIMULATION_MATRIX_STATE_CONFIG_HPP
#define ORES_ANALYTICS_API_DOMAIN_CREDIT_SIMULATION_MATRIX_STATE_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief A credit state a transition matrix spans.
 *
 * ORE names the states a matrix spans in an XML comment inside the Data
 * element, one label per row and column, and indexes the grid by their
 * position. The labels are not derivable from the numbers, so without them
 * the comment cannot be written back and the document does not round trip.
 * Every shipped example declares the same eight labels, so this is a
 * vocabulary rather than free text.
 */
struct credit_simulation_matrix_state_config final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Workspace this record belongs to.
     *
     * Defaults to the Live workspace sentinel.
     */
    boost::uuids::uuid workspace_id = utility::uuid::live_workspace_id();

    /**
     * @brief Surrogate key for the state.
     */
    boost::uuids::uuid id;

    /**
     * @brief The matrix this state belongs to.
     */
    boost::uuids::uuid transition_matrix_id;

    /**
     * @brief The zero-based position of the state in the matrix, which is what the cells index by
     * their from_state and to_state.
     */
    int position = 0;

    /**
     * @brief The state name as ORE writes it, for example Aaa or Default.
     */
    std::string label;

    /**
     * @brief Username of the person who last modified this matrix state.
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
    friend bool operator==(const credit_simulation_matrix_state_config&,
                           const credit_simulation_matrix_state_config&) = default;
};

/**
 * @brief Dispatch-key identifier for credit_simulation_matrix_state_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const credit_simulation_matrix_state_config&) {
    return "ores.analytics.credit_simulation_matrix_state_config";
}

}

#endif
