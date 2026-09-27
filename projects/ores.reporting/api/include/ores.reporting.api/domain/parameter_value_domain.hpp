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
#ifndef ORES_REPORTING_API_DOMAIN_PARAMETER_VALUE_DOMAIN_HPP
#define ORES_REPORTING_API_DOMAIN_PARAMETER_VALUE_DOMAIN_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief The kind of value a configuration parameter holds.
 *
 * Every configuration parameter holds one value, and this says what that value
 * is: a currency, a date, a boolean, an integer, a tenor, a document reference,
 * or a list. Where the value names something in the model, referenced_entity
 * names the entity it points at, so the mapper knows what to write into the
 * document and a reader knows what the value means.
 *
 * A malformed org header makes this loader fail in a misleading way: a line
 * that does not start at column zero, or a stray character before a #+ keyword,
 * leaves the entity type unresolved and the generator writes files called
 * unknown_* rather than reporting the bad line. Check the header first when a
 * model generates unknown.
 */
struct parameter_value_domain final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The stable code for the domain, for example currency or tenor.
     */
    std::string code;

    /**
     * @brief The human-readable name of the domain.
     */
    std::string name;

    /**
     * @brief How a value in this domain is stored: text, numeric, integer, boolean, date, uuid or
     * list.
     */
    std::string storage_kind;

    /**
     * @brief The entity a value in this domain refers to, when it refers to one, for example
     * ores.refdata.currency. Empty when the value stands alone.
     */
    std::string referenced_entity;

    /**
     * @brief Username of the person who last modified this parameter value domain.
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
    friend bool operator==(const parameter_value_domain&, const parameter_value_domain&) = default;
};

/**
 * @brief Dispatch-key identifier for parameter_value_domain, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const parameter_value_domain&) {
    return "ores.reporting.parameter_value_domain";
}

}

#endif
