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
#ifndef ORES_REPORTING_API_DOMAIN_ANALYTIC_TYPE_HPP
#define ORES_REPORTING_API_DOMAIN_ANALYTIC_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief An analytic an ORE run document can ask for.
 *
 * Reference data naming the analytics ORE's run document can ask for: NPV,
 * cashflows, curves, simulation, XVA, initial margin, sensitivity, stress, SIMM
 * and the rest. The element's type attribute is a free string in ORE's schema;
 * this is the vocabulary the shipped run documents actually use, and it is what
 * makes the attribute a foreign key rather than free text.
 *
 * Thirty-five types are in use across the corpus, and their parameter sets barely
 * overlap: xva takes around seventy parameters, simulation sixty, sensitivity
 * thirty, and several types one flag. That is why each type owns a table for its
 * parameters and this table names it, in parameter_entity, rather than every
 * type sharing one wide row.
 */
struct analytic_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The stable code for the analytic, which is the value ORE writes in its type attribute,
     * for example npv or initialMargin.
     */
    std::string code;

    /**
     * @brief The human-readable name of the analytic.
     */
    std::string name;

    /**
     * @brief What the analytic calculates, in one line.
     */
    std::string description;

    /**
     * @brief The entity that holds this analytic's parameters, or nothing when the analytic takes
     * none.
     */
    std::optional<std::string> parameter_entity;

    /**
     * @brief The order the analytic appears in a list.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this analytic type.
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
    friend bool operator==(const analytic_type&, const analytic_type&) = default;
};

/**
 * @brief Dispatch-key identifier for analytic_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const analytic_type&) {
    return "ores.reporting.analytic_type";
}

}

#endif
