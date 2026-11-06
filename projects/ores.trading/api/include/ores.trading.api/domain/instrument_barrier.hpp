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
#ifndef ORES_TRADING_API_DOMAIN_INSTRUMENT_BARRIER_HPP
#define ORES_TRADING_API_DOMAIN_INSTRUMENT_BARRIER_HPP

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
 * One barrier of a product, keyed to the trade, the list that states it and
 * the barrier's ordinal in that list.
 *
 * ORE states a barrier as a BarrierData block: a type, a style, a list of
 * levels and an optional rebate. Fifteen products state one, some state a
 * list of them, and a generic barrier option also states a transatlantic
 * barrier. Every one of them is a row here, so a product with one barrier
 * holds one row, a product with several holds several, and the type, style
 * and rebate belong to each barrier rather than to the product.
 *
 * The levels are instrument_barrier_level rows: a single barrier states
 * one level and a double barrier states two.
 */
struct instrument_barrier final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade this barrier belongs to.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief Which list of the product states the barrier: barrier for the product's barrier or
     * barriers, transatlantic for a generic barrier option's transatlantic barrier.
     */
    std::string barrier_role;

    /**
     * @brief 1-based ordinal of this barrier within its list, in the order the document states it.
     */
    int sequence_number;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief The barrier's type, as Type states it, such as UpAndOut, KnockIn or FixingCap.
     */
    std::string barrier_type;

    /**
     * @brief When the barrier is observed, as Style states it: American or European.
     */
    std::optional<std::string> style;

    /**
     * @brief Whether the barrier triggers at the level itself, as StrictComparison states it.
     */
    std::optional<std::string> strict_comparison;

    /**
     * @brief The amount paid when the barrier triggers, as Rebate states it.
     */
    std::optional<ores::utility::decimal::decimal> rebate;

    /**
     * @brief The currency of the rebate, as RebateCurrency states it.
     */
    std::optional<std::string> rebate_currency;

    /**
     * @brief When the rebate is paid, as RebatePayTime states it: atHit or atExpiry.
     */
    std::optional<std::string> rebate_pay_time;

    /**
     * @brief Whether the barrier is treated as already triggered, as OverrideTriggered states it.
     */
    std::optional<std::string> override_triggered;

    /**
     * @brief Username of the person who last modified this instrument barrier.
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
    friend bool operator==(const instrument_barrier&, const instrument_barrier&) = default;
};

/**
 * @brief Dispatch-key identifier for instrument_barrier, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const instrument_barrier&) {
    return "ores.trading.instrument_barrier";
}

}

#endif
