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
#ifndef ORES_TRADING_API_DOMAIN_RANGE_BOUND_HPP
#define ORES_TRADING_API_DOMAIN_RANGE_BOUND_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <span>
#include <string_view>
#include <type_traits>

namespace ores::trading::domain {

/**
 * @brief One range-bound level of an accumulator document, keyed to the trade and the entry's
 * ordinal.
 *
 * One row per entry in the RangeBounds list an accumulator document
 * states, keyed to the trade and the entry's ordinal. The list holds the
 * levels an accumulator accrues against, so the entry is the unit the
 * document writes and the unit the table stores.
 *
 * The ORE schema states the list as unbounded RangeBound entries and
 * requires at least one; each entry carries an optional start date as an
 * attribute and the optional levels RangeFrom, RangeTo, Leverage,
 * Strike and StrikeAdjustment. The order is the document's order, and
 * the ordinal preserves it, so export re-emits the entries as the document
 * held them.
 *
 * A level is money unless it is a ratio. RangeFrom, RangeTo, Strike
 * and StrikeAdjustment are price levels, so each column is exact decimal, at
 * the family's twelve decimal places: a document that states a level with more
 * digits than that has its value rounded, which is the precision every money
 * column in the component states. Leverage is a multiplier, so the column is a
 * binary double, the same way a basket weight is. A document that states no level
 * leaves the column null rather than a zero it never wrote.
 *
 * The rows are keyed by the trade alone, so any instrument whose document
 * states a RangeBounds list can own them. The FX accumulator is the first
 * to write them. The equity accumulator still carries the list as JSON text
 * and moves onto this table in its own unit.
 */
struct range_bound final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade whose document states this range bound.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Ordinal of this entry within the document's list, counting from one.
     *
     * The schema declares the list unbounded and a document's order is the order it stated, so the
     * ordinal is what identifies a row and preserves that order.
     */
    int sequence_number;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief Date the bound starts, when the document states one.
     *
     * The schema states the date as an attribute of the entry rather than an element, and it is
     * optional. A document that states no start date leaves the column null, and the reverse mapper
     * then writes no attribute back.
     */
    std::optional<std::chrono::year_month_day> start_date;

    /**
     * @brief Lower level of the range the accumulator accrues against, when the document states
     * one.
     */
    std::optional<ores::utility::decimal::decimal> range_from;

    /**
     * @brief Upper level of the range the accumulator accrues against, when the document states
     * one.
     */
    std::optional<ores::utility::decimal::decimal> range_to;

    /**
     * @brief Multiplier the accumulator applies to the accrual at this bound, when the document
     * states one.
     *
     * A leverage is a ratio rather than a money amount, so the column is a binary double for the
     * same reason a basket weight is.
     */
    std::optional<double> leverage;

    /**
     * @brief Strike the bound is measured against, when the document states one.
     */
    std::optional<ores::utility::decimal::decimal> strike;

    /**
     * @brief Adjustment applied to the strike at this bound, when the document states one.
     */
    std::optional<ores::utility::decimal::decimal> strike_adjustment;

    /**
     * @brief Username of the person who last modified this range bound.
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
    friend bool operator==(const range_bound&, const range_bound&) = default;
};

/**
 * @brief The members of range_bound the customer agreed to.
 *
 * Declared by the model's :economic: property on each column.
 */
inline constexpr std::string_view range_bound_economic_fields[] = {
    "range_from",
    "range_to",
    "leverage",
    "strike",
    "strike_adjustment",
};

/**
 * @brief Names the members of range_bound that are economic.
 *
 * The economic digest reads this at compile time and decides nothing by a
 * field's name. A type without this overload cannot be digested.
 */
[[nodiscard]] constexpr std::span<const std::string_view>
economic_fields_of(std::type_identity<range_bound>) {
    return range_bound_economic_fields;
}

/**
 * @brief Dispatch-key identifier for range_bound, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const range_bound&) {
    return "ores.trading.range_bound";
}

}

#endif
