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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_POSITION_OPTION_UNDERLYING_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_POSITION_OPTION_UNDERLYING_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One underlying entry of an equity option position, keyed to the instrument and the entry's
 * ordinal.
 *
 * One row per underlying entry an equity option position names, keyed to
 * the instrument and the entry's ordinal within the document.
 *
 * The ORE schema states an equity option position's members as an unbounded
 * list of Underlying elements, each carrying a shared underlying, an
 * optionData and a Strike. The list order is the document's order and
 * the ordinal preserves it, so export re-emits the entries as the document
 * held them.
 *
 * The position's Quantity stays on the parent instrument: it is a
 * property of the position, not of one entry. The entry itself is the
 * underlying name, the option terms the document states, the strike and,
 * when the document states one, the entry's weight.
 *
 * The strike is a money-role value per D11, so the column holds an exact
 * numeric and not a binary float. A weight is money-role too, and the
 * shared underlying type makes Weight optional, so an absent weight
 * stays null rather than becoming a zero the document never wrote.
 *
 * The corpus states the option terms as LongShort, OptionType,
 * Style and Settlement, so each of those has a column. The entry's
 * exercise date list stays unmodelled: it is a collection of its own, and
 * a single date column would silently drop the rest of the list. That
 * shape belongs to defect 16, the observation schedule.
 *
 * The instrument row carries the trade, the workspace and the party. The
 * entry rows are family-owned and ride the instrument's scope, so no
 * workspace column rides them.
 */
struct equity_position_option_underlying final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the equity position instrument whose document states this entry.
     */
    boost::uuids::uuid instrument_id;

    /**
     * @brief Ordinal of this entry within the document's list, counting from one.
     *
     * The schema declares the list unbounded and a document's order is the order it stated, so the
     * ordinal is what identifies a row and preserves that order.
     */
    int sequence_number;

    /**
     * @brief Name of the underlying equity this entry names.
     *
     * The ORE document states it in the Underlying/Name element; the column refuses an empty name.
     */
    std::string underlying_name;

    /**
     * @brief Strike price of this entry's option.
     *
     * The ORE Strike element is a float on the entry, and a strike is a money-role value per D11,
     * so the column is an exact numeric.
     */
    ores::utility::decimal::decimal strike;

    /**
     * @brief Relative weight of this entry within the position, when the document states one.
     *
     * The ORE Underlying/Weight element is optional, and a weight is a money-role value per D11, so
     * the column is an exact numeric and a document that states no weight leaves it null.
     */
    std::optional<ores::utility::decimal::decimal> weight;

    /**
     * @brief Position direction of this entry's option: Long or Short.
     *
     * The ORE OptionData/LongShort element is required, so the column is not null.
     */
    std::string long_short;

    /**
     * @brief Call or Put, when the document states one.
     *
     * Soft FK to ores_trading_option_types_tbl: the values are the closed ORE optionType set (Call,
     * Put). PR 4 tightens the soft reference into a real foreign key.
     */
    std::string option_type;

    /**
     * @brief European, American, or Bermudan, when the document states one.
     *
     * The ORE element is OptionData/Style.
     *
     * Soft FK to ores_trading_exercise_types_tbl: the values are the closed ORE exerciseStyle set
     * (European, Bermudan, American). PR 4 tightens the soft reference into a real foreign key.
     */
    std::string exercise_type;

    /**
     * @brief Cash or Physical, when the document states one.
     *
     * The ORE element is OptionData/Settlement.
     *
     * Soft FK to ores_trading_settlement_types_tbl: the values are the closed ORE settlementType
     * set (Physical, Cash). PR 4 tightens the soft reference into a real foreign key.
     */
    std::string settlement_type;

    /**
     * @brief Username of the person who last modified this equity position option underlying.
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
    friend bool operator==(const equity_position_option_underlying&,
                           const equity_position_option_underlying&) = default;
};

/**
 * @brief Dispatch-key identifier for equity_position_option_underlying, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_position_option_underlying&) {
    return "ores.trading.equity_position_option_underlying";
}

}

#endif
