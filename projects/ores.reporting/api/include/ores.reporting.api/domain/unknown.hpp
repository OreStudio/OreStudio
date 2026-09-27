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
#ifndef ORES_REPORTING_API_DOMAIN__HPP
#define ORES_REPORTING_API_DOMAIN__HPP

#include <string>
#include "ores.utility/uuid/tenant_id.hpp"
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief 
 *
 * +#+filetags: :model:entity:reporting:
 * +#+brief: One value on a configuration.
 * +#+entity_singular: configuration_parameter
 * +#+entity_plural: configuration_parameters
 * +#+entity_title: Configuration Parameter
 * +#+created: 2026-09-27
 * +#+updated: 2026-09-27
 *
 * One value. The value is text because the definition says what it is: the
 * value domain on parameter_definition decides whether this is a currency, a
 * date, a boolean, a tenor or a reference, and the mapper writes it into the
 * document accordingly. Storing it as text is not the same as storing an
 * untyped blob, because the meaning comes from a foreign key rather than from
 * the shape of the row.
 *
 * position keeps the order the document wrote the parameters in.
 */
struct  final {
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
     * @brief Surrogate key for the value.
     */
    boost::uuids::uuid id;

    /**
     * @brief The configuration the value belongs to.
     */
    boost::uuids::uuid configuration_id;

    /**
     * @brief The definition that says what this value means.
     */
    boost::uuids::uuid parameter_definition_id;

    /**
     * @brief The value, whose type comes from the definition's value domain.
     */
    std::string value;

    /**
     * @brief The order the document wrote the parameter in.
     */
    int position = 0;

    /**
     * @brief Username of the person who last modified this configuration parameter.
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
    friend bool operator==(const &, const &) = default;
};

/**
 * @brief Dispatch-key identifier for , e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const &) {
    return "ores.reporting.";
}

}

#endif
