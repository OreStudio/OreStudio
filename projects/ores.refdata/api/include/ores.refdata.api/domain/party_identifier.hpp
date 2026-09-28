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
#ifndef ORES_REFDATA_API_DOMAIN_PARTY_IDENTIFIER_HPP
#define ORES_REFDATA_API_DOMAIN_PARTY_IDENTIFIER_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief An external identifier for a party under a specific scheme.
 *
 * External identifiers for parties, such as LEI codes, BIC/SWIFT codes,
 * national registration numbers, and tax identifiers. Each party can have
 * multiple identifiers across different schemes.
 */
struct party_identifier final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID uniquely identifying this party identifier.
     *
     * Surrogate key for the party identifier record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party this identifier belongs to.
     *
     * References the parent party record.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The identification scheme for this identifier.
     *
     * References the party_id_scheme lookup table (e.g. LEI, BIC).
     */
    std::string id_scheme;

    /**
     * @brief The identifier value.
     *
     * The actual identifier string within the scheme. Part of the natural key alongside party_id
     * and id_scheme — a party can hold more than one identifier under the same scheme as long as
     * the value differs.
     */
    std::string id_value;

    /**
     * @brief Optional description of this identifier.
     *
     * Free text description providing additional context.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this party identifier.
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
    friend bool operator==(const party_identifier&, const party_identifier&) = default;
};

/**
 * @brief Dispatch-key identifier for party_identifier, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const party_identifier&) {
    return "ores.refdata.party_identifier";
}

}

#endif
