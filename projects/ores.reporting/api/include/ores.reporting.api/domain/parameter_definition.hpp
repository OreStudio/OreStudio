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
#ifndef ORES_REPORTING_API_DOMAIN_PARAMETER_DEFINITION_HPP
#define ORES_REPORTING_API_DOMAIN_PARAMETER_DEFINITION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief A parameter an ORE document accepts, and what its value is.
 *
 * The vocabulary of the run document. ORE writes parameters as a name and a
 * value inside a block; scope names the block (Setup, Markets, an analytic)
 * and subtype narrows it, so a pair of them plus name says which parameter
 * this is. position keeps the order the document wrote, because a document
 * that is not reproducible in order is not reproducible.
 *
 * 165 of the 218 parameter names observed in the shipped corpus belong to
 * exactly one analytic kind, which is why the name alone is not the key.
 */
struct parameter_definition final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the definition.
     */
    boost::uuids::uuid id;

    /**
     * @brief The block the parameter belongs to, for example Setup or Markets.
     */
    std::string scope;

    /**
     * @brief The kind within the block that accepts the parameter, for example the analytic type.
     * Empty when the block is the whole context.
     */
    std::string subtype;

    /**
     * @brief The parameter name as ORE writes it.
     */
    std::string name;

    /**
     * @brief The order the document writes the parameter in.
     */
    int position = 0;

    /**
     * @brief What a value of this parameter is.
     */
    std::string parameter_value_domain_code;

    /**
     * @brief Whether the document must carry the parameter.
     */
    bool is_required = false;

    /**
     * @brief Username of the person who last modified this parameter definition.
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
    friend bool operator==(const parameter_definition&, const parameter_definition&) = default;
};

/**
 * @brief Dispatch-key identifier for parameter_definition, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const parameter_definition&) {
    return "ores.reporting.parameter_definition";
}

}

#endif
