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
#ifndef ORES_TRADING_API_DOMAIN_FLEXI_SWAP_LOWER_NOTIONAL_HPP
#define ORES_TRADING_API_DOMAIN_FLEXI_SWAP_LOWER_NOTIONAL_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One lower notional bound of a flexi swap, keyed to the trade and the bound's ordinal.
 *
 * One row per notional a flexi swap's lower bound block states, keyed to the
 * instrument and the notional's ordinal across the whole block.
 *
 * The ORE schema states the lower bounds as an unbounded list of blocks. Each
 * block names a currency once and holds a list of dated notionals. The column
 * bound_number is the block's position in the document, and the row carries the
 * block's currency, so the export regroups the rows into the blocks the document
 * held. A notional that states no date applies from the start of the swap, as in
 * the leg notionals. The document may state a notional of zero, so the amount
 * check allows it.
 */
struct flexi_swap_lower_notional final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade whose flexi swap states this lower notional.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Ordinal of this notional across all the lower bound blocks, counting from one.
     *
     * The schema declares the lists unbounded and a document's order is the order it stated, so the
     * ordinal identifies a row and preserves that order.
     */
    int sequence_number;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief Ordinal of the lower bound block this notional belongs to, counting from one.
     *
     * Rows that share a bound number share a block, so export writes them back under one
     * LowerNotionalBounds element.
     */
    int bound_number = 0;

    /**
     * @brief Currency the lower bound block states, when the document states one.
     *
     * Every row of a block carries the same value.
     */
    std::optional<std::string> currency;

    /**
     * @brief The day this notional starts to apply.
     *
     * Null when the document states the notional without a start date.
     */
    std::optional<std::chrono::year_month_day> start_date;

    /**
     * @brief The lower bound on the swap's notional from start_date onwards.
     */
    ores::utility::decimal::decimal notional;

    /**
     * @brief Username of the person who last modified this flexi swap lower notional.
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
    friend bool operator==(const flexi_swap_lower_notional&,
                           const flexi_swap_lower_notional&) = default;
};

/**
 * @brief Dispatch-key identifier for flexi_swap_lower_notional, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const flexi_swap_lower_notional&) {
    return "ores.trading.flexi_swap_lower_notional";
}

}

#endif
