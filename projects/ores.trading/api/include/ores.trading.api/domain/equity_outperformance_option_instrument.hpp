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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_OUTPERFORMANCE_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_OUTPERFORMANCE_OPTION_INSTRUMENT_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One row per equity outperformance option, keyed by the trade.
 *
 * One row per equity outperformance option, keyed by the trade.
 *
 * The option is on the relative performance of two equities, so the pair is the
 * subject matter and both names have a column. That is why this product cannot
 * live in equity_option_instruments, whose single underlying_name states one.
 *
 * The two initial prices are the levels the payoff measures the return from, and
 * the strike return is the threshold the relative return must beat. Both are
 * amounts and rates that settle cash, so both are exact decimals. The return in
 * particular is a proportion that multiplies money, and this family holds a rate
 * as an exact decimal for that reason.
 *
 * The knock prices are levels the document may omit, so both are nullable.
 *
 * The table is named equity_outperformance_options rather than
 * equity_outperformance_option_instruments. The derived name is one character
 * over the limit the generator allows once the trigger and policy suffixes are
 * counted, and PostgreSQL would truncate it mid-word.
 */
struct equity_outperformance_option_instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade this option belongs to, and the row's key.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief Name of the first equity the relative return is measured on.
     */
    std::string underlying1_name;

    /**
     * @brief Name of the second equity, the one the first is measured against.
     */
    std::string underlying2_name;

    /**
     * @brief Currency of the notional and the payoff.
     */
    std::string currency;

    /**
     * @brief The option's notional. Money, so an exact numeric.
     */
    ores::utility::decimal::decimal notional;

    /**
     * @brief Whether the holder is long or short the option.
     */
    std::string long_short;

    /**
     * @brief The first underlying's level at the trade date, which the payoff measures its return
     * from. A price is money, so an exact numeric.
     */
    ores::utility::decimal::decimal initial_price1;

    /**
     * @brief The second underlying's level at the trade date.
     */
    ores::utility::decimal::decimal initial_price2;

    /**
     * @brief The relative return the holder must beat for the option to pay.
     *
     * A proportion that multiplies money, and this family holds a rate as an exact decimal.
     */
    ores::utility::decimal::decimal strike_return;

    /**
     * @brief The level that activates the option, when the document states one.
     */
    std::optional<ores::utility::decimal::decimal> knock_in_price;

    /**
     * @brief The level that extinguishes the option, when the document states one.
     */
    std::optional<ores::utility::decimal::decimal> knock_out_price;

    /**
     * @brief Currency of the first initial price, when the document states it apart from the
     * notional's.
     */
    std::optional<std::string> initial_price_currency1;

    /**
     * @brief Currency of the second initial price, when the document states it.
     */
    std::optional<std::string> initial_price_currency2;

    /**
     * @brief Username of the person who last modified this equity outperformance option instrument.
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
    friend bool operator==(const equity_outperformance_option_instrument&,
                           const equity_outperformance_option_instrument&) = default;
};

/**
 * @brief Dispatch-key identifier for equity_outperformance_option_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view
entity_type_of(const equity_outperformance_option_instrument&) {
    return "ores.trading.equity_outperformance_option_instrument";
}

}

#endif
