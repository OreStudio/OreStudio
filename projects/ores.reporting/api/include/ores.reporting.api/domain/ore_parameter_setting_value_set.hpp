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
#ifndef ORES_REPORTING_API_DOMAIN_ORE_PARAMETER_SETTING_VALUE_SET_HPP
#define ORES_REPORTING_API_DOMAIN_ORE_PARAMETER_SETTING_VALUE_SET_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief The value set one setting takes.
 *
 * The junction between a setting and the set of values it allows. It is keyed by
 * the setting, (scope, name), and not by a parameter row, because the contract
 * holds one row per product type and the allowed values are the same for all of
 * them. A setting has one set today, so the pair is unique; the junction is a
 * table so a setting may draw from more than one set later without a migration.
 *
 * The code is the scope and the name joined with a colon, for example
 * EngineParameters:Training.BasisFunction.
 */
struct ore_parameter_setting_value_set final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The scope and the setting name joined with a colon, for example
     * EngineParameters:Training.BasisFunction.
     */
    std::string code;

    /**
     * @brief The scope of the setting: ModelParameters, EngineParameters or GlobalParameters.
     */
    std::string scope;

    /**
     * @brief The setting's name as the ORE document holds it, for example Training.BasisFunction.
     */
    std::string name;

    /**
     * @brief The set that states the setting's allowed values. Soft foreign key to
     * ores_reporting_ore_parameter_value_sets_tbl.
     */
    std::string value_set_code;

    /**
     * @brief Username of the person who last modified this ore parameter setting value set.
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
    friend bool operator==(const ore_parameter_setting_value_set&,
                           const ore_parameter_setting_value_set&) = default;
};

/**
 * @brief Dispatch-key identifier for ore_parameter_setting_value_set, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ore_parameter_setting_value_set&) {
    return "ores.reporting.ore_parameter_setting_value_set";
}

}

#endif
