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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_UNDERLYING_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_UNDERLYING_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief
 *
 * One underlying of a product, keyed to the trade, the list that states it and
 * the underlying's ordinal in that list.
 *
 * ORE states an underlying as an Underlying block of its underlying type:
 * a type and a name, and optionally an identifier type, a currency, an
 * exchange, a weight, a price type and the commodity future terms. Twenty-four
 * places in ORE's schema state one, some once and some as a list. Every block
 * is a row here, so a product with one underlying holds one row and a basket
 * or a position holds one per member.
 *
 * A product's fact row keeps the first underlying's name as its summary; the
 * rows keep everything the document states.
 */
struct instrument_underlying final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade this underlying belongs to.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Which list of the product states the underlying: underlying for the product's
     * Underlying or Underlyings.
     */
    std::string underlying_role;

    /**
     * @brief 1-based ordinal of this underlying within its list, in the order the document states
     * it.
     */
    int sequence_number;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief The kind of underlying, as Type states it, such as Equity, FX, Commodity or Bond.
     */
    std::string underlying_type;

    /**
     * @brief The underlying's name, as Name states it.
     */
    std::string name;

    /**
     * @brief The scheme the name is stated in, as IdentifierType states it, such as RIC or ISIN.
     */
    std::optional<std::string> identifier_type;

    /**
     * @brief The underlying's currency, as Currency states it.
     */
    std::optional<std::string> currency;

    /**
     * @brief The exchange the underlying trades on, as Exchange states it.
     */
    std::optional<std::string> exchange;

    /**
     * @brief The underlying's weight in its list, as Weight states it.
     */
    std::optional<ores::utility::decimal::decimal> weight;

    /**
     * @brief The commodity price the underlying is observed at, as PriceType states it: Spot or
     * FutureSettlement.
     */
    std::optional<std::string> price_type;

    /**
     * @brief The commodity future contract months ahead of the observation, as FutureMonthOffset
     * states it.
     */
    std::optional<int> future_month_offset;

    /**
     * @brief The days before expiry the commodity future rolls, as DeliveryRollDays states it.
     */
    std::optional<int> delivery_roll_days;

    /**
     * @brief The calendar the roll days count in, as DeliveryRollCalendar states it.
     */
    std::optional<std::string> delivery_roll_calendar;

    /**
     * @brief The commodity future's expiry date, as FutureExpiryDate states it.
     */
    std::optional<std::string> future_expiry_date;

    /**
     * @brief The commodity future's contract month, as FutureContractMonth states it.
     */
    std::optional<std::string> future_contract_month;

    /**
     * @brief How a commodity price between contracts is interpolated, as Interpolation states it.
     */
    std::optional<std::string> interpolation;

    /**
     * @brief The adjustment applied to the bond price, as BidAskAdjustment states it.
     */
    std::optional<ores::utility::decimal::decimal> bid_ask_adjustment;

    /**
     * @brief Username of the person who last modified this instrument underlying.
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
    friend bool operator==(const instrument_underlying&, const instrument_underlying&) = default;
};

/**
 * @brief Dispatch-key identifier for instrument_underlying, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const instrument_underlying&) {
    return "ores.trading.instrument_underlying";
}

}

#endif
