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
#ifndef ORES_TRADING_API_DOMAIN_SWAP_LEG_RATE_HPP
#define ORES_TRADING_API_DOMAIN_SWAP_LEG_RATE_HPP

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
 * One numbered rate or spread of a swap leg, dated from the day it applies. A
 * leg whose rate steps states one row per step; a leg with a single rate states
 * one row.
 *
 * ORE states a fixed leg's rate as a list on legData/FixedLegData/Rates and a
 * floating leg's spread as a list on legData/FloatingLegData/Spreads. Both are
 * a list of values that vary over the leg's life, so both land here and
 * rate_role says which arm stated the row. The leg itself carries no rate and
 * no spread, because a single column cannot hold a schedule.
 *
 * rate_role is data, not a table: the closed set is fixed and spread, and
 * the schema states it.
 *
 * The row is a child of the leg rather than an entity with a life of its own:
 * swap_leg names the trade and the ordinal, and this row adds its role and its
 * own ordinal within that leg.
 */
struct swap_leg_rate final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade whose leg this rate belongs to.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief 1-based ordinal of the leg within the trade, matching the leg row's own.
     */
    int leg_number;

    /**
     * @brief Which arm of the leg stated this row: fixed for a fixed leg's rate, spread for a
     * floating leg's spread.
     */
    std::string rate_role;

    /**
     * @brief 1-based ordinal of this rate within the leg, in the order the document states it.
     */
    int sequence_number;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief The day this rate starts to apply.
     *
     * Null when the leg states a single undated rate, which is how ORE states the common case.
     */
    std::optional<std::chrono::year_month_day> start_date;

    /**
     * @brief The rate or the spread, as a decimal: 0.05 is five per cent.
     */
    double value;

    /**
     * @brief Username of the person who last modified this swap leg rate.
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
    friend bool operator==(const swap_leg_rate&, const swap_leg_rate&) = default;
};

/**
 * @brief Dispatch-key identifier for swap_leg_rate, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const swap_leg_rate&) {
    return "ores.trading.swap_leg_rate";
}

}

#endif
