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
#ifndef ORES_REPORTING_API_DOMAIN_ORE_PARAMETER_VALUE_SET_CATALOGUE_HPP
#define ORES_REPORTING_API_DOMAIN_ORE_PARAMETER_VALUE_SET_CATALOGUE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief A catalogue whose current rows are the allowed values of a parameter.
 *
 * A parameter whose values are other ORE Studio data names a catalogue here: the
 * tenors, the currencies, the currency pairs, the sensitivity templates, the
 * instrument codes, the calendars, the day counters, the conventions. The values
 * are the catalogue's current rows, read through a view that presents the value
 * and the label, so nothing is copied into the parameter tables and a row retired
 * from the catalogue is refused by the same read.
 *
 * Only a registered relation can be read, and its shape is checked when the
 * registry loads. A catalogue is data, so adding one is a row here and the view it
 * names.
 */
struct ore_parameter_value_set_catalogue final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The catalogue's code, for example refdata.tenor or analytics.sensitivity_template.
     */
    std::string code;

    /**
     * @brief The name a person reads, for example Tenors.
     */
    std::string name;

    /**
     * @brief The view that presents the catalogue as values and labels, for example
     * ores_refdata_tenor_catalogue_v. It must expose value, label and is_active.
     */
    std::string relation;

    /**
     * @brief What the catalogue holds and when a parameter draws on it.
     */
    std::string description;

    /**
     * @brief Whether a value set may still name the catalogue.
     */
    bool is_active = false;

    /**
     * @brief Username of the person who last modified this ore parameter value set catalogue.
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
    friend bool operator==(const ore_parameter_value_set_catalogue&,
                           const ore_parameter_value_set_catalogue&) = default;
};

/**
 * @brief Dispatch-key identifier for ore_parameter_value_set_catalogue, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const ore_parameter_value_set_catalogue&) {
    return "ores.reporting.ore_parameter_value_set_catalogue";
}

}

#endif
