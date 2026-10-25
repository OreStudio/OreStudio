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
#ifndef ORES_REPORTING_API_DOMAIN_ORE_PARAMETER_VALUE_SET_VALUE_HPP
#define ORES_REPORTING_API_DOMAIN_ORE_PARAMETER_VALUE_SET_VALUE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief One value of an enumerated parameter set.
 *
 * One value of an enumerated set, with the label a person reads, where it sits in
 * the combo and whether it may still be chosen. The contract is the one place a
 * value list lives: the per-pair settings table no longer carries one.
 */
struct ore_parameter_value_set_value final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The set and the value joined with a colon, for example polynom_type:Chebyshev2nd.
     */
    std::string code;

    /**
     * @brief The set this value belongs to. Soft foreign key to
     * ores_reporting_ore_parameter_value_sets_tbl.
     */
    std::string value_set_code;

    /**
     * @brief The string a configuration holds, for example Chebyshev2nd.
     */
    std::string value;

    /**
     * @brief The words a person reads in the combo, for example Chebyshev, second kind.
     */
    std::string label;

    /**
     * @brief What choosing this value means.
     */
    std::string description;

    /**
     * @brief Where the value sits in the combo, counting from zero.
     */
    int display_order = 0;

    /**
     * @brief Whether the value may still be chosen. A value turned inactive stays on the
     * configurations that already hold it and is refused for a new one.
     */
    bool is_active = false;

    /**
     * @brief Username of the person who last modified this ore parameter value set value.
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
    friend bool operator==(const ore_parameter_value_set_value&,
                           const ore_parameter_value_set_value&) = default;
};

/**
 * @brief Dispatch-key identifier for ore_parameter_value_set_value, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ore_parameter_value_set_value&) {
    return "ores.reporting.ore_parameter_value_set_value";
}

}

#endif
