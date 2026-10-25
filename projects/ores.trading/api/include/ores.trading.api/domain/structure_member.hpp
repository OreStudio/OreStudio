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
#ifndef ORES_TRADING_API_DOMAIN_STRUCTURE_MEMBER_HPP
#define ORES_TRADING_API_DOMAIN_STRUCTURE_MEMBER_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief The place one trade holds inside a structure.
 *
 * The link between a structure and one of its legs. The trade is not owned by
 * the deal: it points at it, so the same trade can be unlinked from one
 * structure and linked to another without being rewritten.
 *
 * The row is keyed by the trade and is temporal, which says the rule directly: a
 * trade sits in at most one structure at a time, and unlinking closes the row
 * rather than deleting it. The history of what a trade belonged to survives.
 *
 * The member copies the party and the counterparty from its structure, and the
 * pins hold the copies to their source, so a leg cannot disagree with the deal
 * it belongs to. The insert trigger holds a leg to the roles its template
 * allows, and to the number of legs the template states for that role.
 */
struct structure_member final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade that is a leg of the deal. Keying the table on it is the rule that a trade
     * belongs to one structure at a time.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief The deal the trade is a leg of.
     */
    boost::uuids::uuid structure_id;

    /**
     * @brief The part the leg plays, which the structure's template must allow: the body of a
     * butterfly, the wing of a risk reversal, the premium leg.
     */
    std::string role;

    /**
     * @brief The leg's ordinal within its role, counting from one.
     */
    int sequence_number = 0;

    /**
     * @brief Username of the person who last modified this structure member.
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
    friend bool operator==(const structure_member&, const structure_member&) = default;
};

/**
 * @brief Dispatch-key identifier for structure_member, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const structure_member&) {
    return "ores.trading.structure_member";
}

}

#endif
