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
#include "ores.utility/digest/canonical_form.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

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
 * The ORE schema states the list as unbounded RangeBound entries. The
 * RangeBounds wrapper is optional, but the schema requires at least one entry
 * when a document states it, and this binding writes the wrapper
 * unconditionally, so a trade that holds no bound cannot be written back. Each
 * entry carries an optional start date as an attribute and the optional levels
 * RangeFrom, RangeTo, Leverage, Strike and StrikeAdjustment. The order
 * is the document's order, and the ordinal preserves it, so export re-emits the
 * entries as the document held them.
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
    int sequence_number{};

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
 * @brief The canonical text of the members of range_bound the customer agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const range_bound& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "range_from", v.range_from);
    ores::utility::digest::append_member(out, "range_to", v.range_to);
    ores::utility::digest::append_member(out, "leverage", v.leverage);
    ores::utility::digest::append_member(out, "strike", v.strike);
    ores::utility::digest::append_member(out, "strike_adjustment", v.strike_adjustment);
    out.push_back('}');
    return out;
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
