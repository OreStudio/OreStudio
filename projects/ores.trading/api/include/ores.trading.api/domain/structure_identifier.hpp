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
#ifndef ORES_TRADING_API_DOMAIN_STRUCTURE_IDENTIFIER_HPP
#define ORES_TRADING_API_DOMAIN_STRUCTURE_IDENTIFIER_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>

namespace ores::trading::domain {

/**
 * @brief An identifier of a structure under one scheme (UTI, USI, Internal, ORE).
 *
 * One identifier a trade is known by under one scheme: a UTI, a USI, the
 * firm's internal reference, or the id an ORE document gave it. Each scheme
 * is a [[id:20D446E8-EA13-47AA-BE0C-FDDD7CF428F3][trade id type]] code, and a trade has at most one
 * value per scheme. The firm's own id is the anchor's UUID, the trade id, and is not repeated here.
 *
 * The row is keyed by the trade and references the [[id:4304A441-E532-45FB-837A-378F13693CAE][trade
 * anchor]] with a database foreign key. It copies the anchor's party, pinned to the anchor, because
 * row-level security needs the party on every row. The issuer is the party or counterparty that
 * issued the value, when it is known.
 */
struct structure_identifier final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade this identifier belongs to.
     */
    boost::uuids::uuid structure_id;

    /**
     * @brief The scheme the value belongs to (e.g. UTI, USI, Internal, ORE).
     */
    std::string id_type;

    /**
     * @brief The identifier value under the scheme.
     */
    std::string id_value;

    /**
     * @brief The party or counterparty that issued the value. Absent when the issuer is unknown.
     */
    std::optional<boost::uuids::uuid> issuing_party_id;

    /**
     * @brief Username of the person who last modified this trade identifier.
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
    friend bool operator==(const structure_identifier&, const structure_identifier&) = default;
};

/**
 * @brief The members of structure_identifier the customer agreed to.
 *
 * Declared by the model's :economic: property on each column.
 */
inline constexpr std::string_view structure_identifier_economic_fields[] = {
    "id_type",
    "id_value",
};

/**
 * @brief Names the members of structure_identifier that are economic.
 *
 * The economic digest reads this at compile time and decides nothing by a
 * field's name. A type without this overload cannot be digested.
 */
[[nodiscard]] constexpr std::span<const std::string_view>
economic_fields_of(std::type_identity<structure_identifier>) {
    return structure_identifier_economic_fields;
}

/**
 * @brief Dispatch-key identifier for structure_identifier, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const structure_identifier&) {
    return "ores.trading.structure_identifier";
}

}

#endif
