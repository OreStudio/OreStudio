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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_LINK_TYPE_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_LINK_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief The reason two trades are linked (e.g. Novation, Roll, Exercise).
 *
 * Reference data table defining the reasons two trades are joined. Each
 * type names the role of each end and says whether the link carries an
 * economic effect, which is what tells a trade event from an annotation.
 *
 * Examples: 'Novation', 'Roll', 'Exercise', 'Hedge'.
 */
struct trade_link_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The link type code.
     *
     * Examples: 'Novation', 'Roll', 'Exercise'.
     */
    std::string code;

    /**
     * @brief What the link type means and when it is recorded.
     */
    std::string description;

    /**
     * @brief The role the from end of the link plays, in the type's own words: the original trade a
     * close-out ends, the transferred position a novation moves, the rolled trade a roll replaces.
     */
    std::string from_role;

    /**
     * @brief The role the to end of the link plays: the closing trade, the new counterparty's
     * trade, the replacement a roll books.
     */
    std::string to_role;

    /**
     * @brief True when the link carries an economic effect, which makes it a trade event that is
     * confirmed and settled. False when it is an annotation that changes no term.
     */
    bool has_economic_effect = false;

    /**
     * @brief Username of the person who last modified this trade link type.
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
    friend bool operator==(const trade_link_type&, const trade_link_type&) = default;
};

/**
 * @brief Dispatch-key identifier for trade_link_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const trade_link_type&) {
    return "ores.trading.trade_link_type";
}

}

#endif
