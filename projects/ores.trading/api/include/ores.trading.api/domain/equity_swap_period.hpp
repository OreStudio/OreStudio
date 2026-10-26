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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_SWAP_PERIOD_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_SWAP_PERIOD_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <span>
#include <string_view>
#include <type_traits>

namespace ores::trading::domain {

/**
 * @brief One determination period of an equity worst-of basket swap, with the levels it states.
 *
 * One row per determination period of an equity worst-of basket swap, keyed to
 * the trade and the period's ordinal within the document.
 *
 * The levels are a list. A list is rows of a child table rather than an encoded
 * string, so the periods live here and not in the knock_out_levels_json and
 * fixed_trigger_levels_json columns that used to hold them. Both columns are
 * retired with this table.
 *
 * The ORE document states KnockOutLevels and FixedTriggerLevels as two
 * unbounded lists that run beside the determination schedule, one entry per
 * date. They are therefore per period and not per constituent, which is why
 * they are not rows of equity_basket_constituent: a basket of two underlyings
 * over four dates states two names and four levels.
 *
 * The trade is the key. A period is reached through the trade that owns it and
 * by nothing else, so the pair of trade_id and sequence_number is the
 * primary key.
 */
struct equity_swap_period final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade the period belongs to.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief The period's 1-based ordinal in the document's order.
     *
     * A level list and a date list are two lists that run in step, so the ordinal is what pairs a
     * level with the date it belongs to.
     */
    int sequence_number;

    /**
     * @brief The determination date the period's levels are observed on, when the document states a
     * schedule.
     */
    std::optional<std::chrono::year_month_day> period_date;

    /**
     * @brief The knock-out level observed on the period, when the document states one.
     *
     * A level is a continuous quantity and not money, so it is a binary float, as the constituent's
     * weight is.
     */
    std::optional<double> knock_out_level;

    /**
     * @brief The fixed trigger level observed on the period, when the document states one.
     */
    std::optional<double> fixed_trigger_level;

    /**
     * @brief Username of the person who last modified this equity swap period.
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
    friend bool operator==(const equity_swap_period&, const equity_swap_period&) = default;
};

/**
 * @brief The members of equity_swap_period the customer agreed to.
 *
 * Declared by the model's :economic: property on each column.
 */
inline constexpr std::string_view equity_swap_period_economic_fields[] = {
    "period_date",
    "knock_out_level",
    "fixed_trigger_level",
};

/**
 * @brief Names the members of equity_swap_period that are economic.
 *
 * The economic digest reads this at compile time and decides nothing by a
 * field's name. A type without this overload cannot be digested.
 */
[[nodiscard]] constexpr std::span<const std::string_view>
economic_fields_of(std::type_identity<equity_swap_period>) {
    return equity_swap_period_economic_fields;
}

/**
 * @brief Dispatch-key identifier for equity_swap_period, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_swap_period&) {
    return "ores.trading.equity_swap_period";
}

}

#endif
