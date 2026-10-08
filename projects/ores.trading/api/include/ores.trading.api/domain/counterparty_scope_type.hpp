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
#ifndef ORES_TRADING_API_DOMAIN_COUNTERPARTY_SCOPE_TYPE_HPP
#define ORES_TRADING_API_DOMAIN_COUNTERPARTY_SCOPE_TYPE_HPP

#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Who the firm faces on a trade.
 *
 * Reference data table holding who the firm faces on a trade, and so what
 * obligation the booking creates: external (a party outside the group),
 * inter_entity (another legal entity of the same group) and
 * intra_entity (another book in the same legal entity and branch).
 *
 * The set is closed, and the C++ enum domain::counterparty_scope holds the same
 * values. A code with no enum value cannot be read back, so the table is
 * immutable: it is seeded once and never changes at run time. Because it
 * is immutable, the trade anchor references it with a database foreign
 * key rather than a trigger check. The table has no tenant: the set is the
 * same for every tenant.
 */
struct counterparty_scope_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Unique counterparty scope code.
     *
     * Examples: 'external', 'intra_entity'.
     */
    std::string code;

    /**
     * @brief What the code means.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this counterparty scope type.
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
    friend bool operator==(const counterparty_scope_type&,
                           const counterparty_scope_type&) = default;
};

/**
 * @brief Dispatch-key identifier for counterparty_scope_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const counterparty_scope_type&) {
    return "ores.trading.counterparty_scope_type";
}

}

#endif
