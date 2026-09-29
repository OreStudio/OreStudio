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
#ifndef ORES_TRADING_API_DOMAIN_COMMODITY_BASKET_CONSTITUENT_HPP
#define ORES_TRADING_API_DOMAIN_COMMODITY_BASKET_CONSTITUENT_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One constituent of a commodity basket instrument, keyed to the trade and the constituent's
 * ordinal.
 *
 * One row per underlying a commodity basket product names, keyed to the
 * instrument and the constituent's ordinal within the document.
 *
 * The ORE schema states a commodity basket's members as an unbounded list of
 * Underlying elements, each carrying a Name and an optional Weight.
 * The list order is the document's order and the ordinal preserves it, so
 * export re-emits the constituents as the document held them.
 *
 * A constituent is a name and, when the document states one, a weight. The
 * weight is money-role decimal, so the column holds an exact numeric and not
 * a binary float. The document may state no weight, and then the column is
 * null rather than a zero the document never wrote.
 *
 * The trade row carries the workspace and the party. The
 * constituent rows are family-owned and ride the trade's scope, so no
 * workspace column rides them.
 */
struct commodity_basket_constituent final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade whose commodity basket document states this constituent.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Ordinal of this constituent within the document's list, counting from one.
     *
     * The schema declares the list unbounded and a document's order is the order it stated, so the
     * ordinal is what identifies a row and preserves that order.
     */
    int sequence_number;

    /**
     * @brief Code or name of the underlying this constituent names.
     *
     * The ORE document states it in the Underlying/Name element; the column refuses an empty name.
     */
    std::string underlying_code;

    /**
     * @brief Relative weight of this constituent in the basket, when the document states one.
     *
     * The ORE Weight element is optional. A document that states no weight leaves this column null,
     * and the reverse mapper then writes no Weight element back.
     */
    std::optional<ores::utility::decimal::decimal> weight;

    /**
     * @brief Username of the person who last modified this commodity basket constituent.
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
    friend bool operator==(const commodity_basket_constituent&,
                           const commodity_basket_constituent&) = default;
};

/**
 * @brief Dispatch-key identifier for commodity_basket_constituent, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const commodity_basket_constituent&) {
    return "ores.trading.commodity_basket_constituent";
}

}

#endif
