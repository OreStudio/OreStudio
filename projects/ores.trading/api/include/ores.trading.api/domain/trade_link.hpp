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
#ifndef ORES_TRADING_API_DOMAIN_TRADE_LINK_HPP
#define ORES_TRADING_API_DOMAIN_TRADE_LINK_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief A typed, directed relation between two trades (e.g. a close-out, a roll).
 *
 * One relation between two trades, with a direction, a type and nothing
 * else. A close-out answers a live trade, a roll replaces one, an exercise
 * produces one. The link holds no cash flow, no amount and no state: the
 * [[id:20D446E8-EA13-47AA-BE0C-FDDD7CF428F3][trade id type]] pattern of a
 * typed reference applies here as well, with the type carrying the role of
 * each end.
 *
 * The row is keyed by its two ends and its type, and the type is a
 * [[id:8C8D3355-1797-4D8E-A47D-07D4402B2794][trade link type]] code. It
 * records the trade activity that made it, because a link is created by an
 * amendment like anything else, and it copies the party of the from end so
 * row level security can see it on every row.
 *
 * A process that created a link records the run on the trade group, not
 * here: a link holds nothing and stands alone.
 */
struct trade_link final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade the link starts at: the original a close-out ends, the transferred position
     * a novation moves, the rolled trade a roll replaces.
     */
    boost::uuids::uuid from_trade_id;

    /**
     * @brief The trade the link ends at: the closing trade, the new counterparty's trade, the
     * replacement a roll books.
     */
    boost::uuids::uuid to_trade_id;

    /**
     * @brief The reason the two trades are joined, from the trade link types.
     */
    std::string link_type;

    /**
     * @brief The activity that made the link.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief Username of the person who last modified this trade link.
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
    friend bool operator==(const trade_link&, const trade_link&) = default;
};

/**
 * @brief Dispatch-key identifier for trade_link, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const trade_link&) {
    return "ores.trading.trade_link";
}

}

#endif
