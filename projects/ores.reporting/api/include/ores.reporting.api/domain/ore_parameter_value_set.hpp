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
#ifndef ORES_REPORTING_API_DOMAIN_ORE_PARAMETER_VALUE_SET_HPP
#define ORES_REPORTING_API_DOMAIN_ORE_PARAMETER_VALUE_SET_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief The set of values a parameter may hold.
 *
 * A parameter states its storage type in the value domain and its allowed values
 * here. A set is enumerated, in which case its values are rows of
 * ore_parameter_value_set_value, or it is a catalogue, in which case the values
 * are the current rows of another ORE Studio catalogue named by
 * ore_parameter_value_set_catalogue. A range and a regular expression carry
 * their rules on the set itself. A boolean is the built-in pair true and false.
 *
 * The set is shared by every parameter that names it, so one list of values serves
 * every product type and every pair that takes the setting.
 */
struct ore_parameter_value_set final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The stable code of the set, for example polynom_type or refdata.tenor.
     */
    std::string code;

    /**
     * @brief The name a person reads, for example Basis function system.
     */
    std::string name;

    /**
     * @brief What the values of this set choose.
     */
    std::string description;

    /**
     * @brief Where the values come from: enumerated, catalogue, range, regex or boolean. The screen
     * draws a combo, a bounded number field, a text field or a flag from it.
     */
    std::string source_kind;

    /**
     * @brief The catalogue, when the kind is catalogue. Soft foreign key to
     * ores_reporting_ore_parameter_value_set_catalogues_tbl.
     */
    std::string catalogue_code;

    /**
     * @brief The lowest value a range accepts. Empty when the range has no floor.
     */
    std::optional<double> minimum;

    /**
     * @brief The highest value a range accepts. Empty when the range has no ceiling.
     */
    std::optional<double> maximum;

    /**
     * @brief The regular expression a value must match, when the kind is regex.
     */
    std::string pattern;

    /**
     * @brief Whether the order of an enumerated set means something, so the screen keeps the order
     * the values rows state.
     */
    bool is_ordered = false;

    /**
     * @brief Username of the person who last modified this ore parameter value set.
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
    friend bool operator==(const ore_parameter_value_set&,
                           const ore_parameter_value_set&) = default;
};

/**
 * @brief Dispatch-key identifier for ore_parameter_value_set, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ore_parameter_value_set&) {
    return "ores.reporting.ore_parameter_value_set";
}

}

#endif
