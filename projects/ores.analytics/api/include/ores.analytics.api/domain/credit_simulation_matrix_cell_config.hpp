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
#ifndef ORES_ANALYTICS_API_DOMAIN_CREDIT_SIMULATION_MATRIX_CELL_CONFIG_HPP
#define ORES_ANALYTICS_API_DOMAIN_CREDIT_SIMULATION_MATRIX_CELL_CONFIG_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::analytics::domain {

/**
 * @brief One cell of a credit migration transition matrix.
 *
 * ORE writes a transition matrix as one Data element whose text is a
 * whitespace or comma separated square grid, with optional t0 and t1
 * attributes naming its states. A matrix carried that way is not queryable
 * and not diffable, so the model holds one row per cell: the row names the
 * matrix, the state it moves from and the state it moves to, and carries
 * the probability.
 */
struct credit_simulation_matrix_cell_config final {
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
     * @brief Surrogate key for the cell.
     */
    boost::uuids::uuid id;

    /**
     * @brief The matrix this cell belongs to.
     */
    boost::uuids::uuid transition_matrix_id;

    /**
     * @brief The state the entity migrates from, zero-based, in the order the matrix declares its
     * states.
     */
    int from_state = 0;

    /**
     * @brief The state the entity migrates to, zero-based.
     */
    int to_state = 0;

    /**
     * @brief The probability of moving from from_state to to_state over the matrix's period. ORE
     * writes it as text inside the grid, so it is parsed on import and formatted on export.
     */
    double probability;

    /**
     * @brief Username of the person who last modified this transition matrix cell.
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
    friend bool operator==(const credit_simulation_matrix_cell_config&,
                           const credit_simulation_matrix_cell_config&) = default;
};

/**
 * @brief Dispatch-key identifier for credit_simulation_matrix_cell_config, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const credit_simulation_matrix_cell_config&) {
    return "ores.analytics.credit_simulation_matrix_cell_config";
}

}

#endif
