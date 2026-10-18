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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_TARF_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_TARF_INSTRUMENT_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One row per equity target redemption forward, keyed by the trade.
 *
 * One row per equity target redemption forward, keyed by the trade.
 *
 * The product's own subject matter is the target: the amount the holder aims to
 * redeem and the points that measure progress towards it. An accumulator has
 * neither, which is why the code cannot keep routing to
 * equity_accumulator_instruments.
 *
 * The fixing amount and the target amount are money and so exact decimals. The
 * strike is money too and is optional, because the document may state its
 * strikes in a list instead, and the barrier that ends the product belongs with
 * the shared strike child rather than here. Target points are a count of
 * observations rather than an amount, so they stay a continuous quantity.
 *
 * The observation and settlement schedule belongs to the shared schedule
 * children, so this table keeps only what is particular to the product.
 */
struct equity_tarf_instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade this forward belongs to, and the row's key.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief Name of the equity underlying the forward is on.
     */
    std::string underlying_name;

    /**
     * @brief Currency of the fixing amount, the target and the strike.
     */
    std::string currency;

    /**
     * @brief The amount fixed at each observation. Money, so an exact numeric.
     */
    ores::utility::decimal::decimal fixing_amount;

    /**
     * @brief The amount the holder aims to redeem, when the document states one.
     */
    std::optional<ores::utility::decimal::decimal> target_amount;

    /**
     * @brief The number of observations that reaches the target, when the document states it. A
     * count of observations rather than an amount, so a continuous quantity and not money.
     */
    std::optional<double> target_points;

    /**
     * @brief The single strike, when the document states one instead of a strike list.
     */
    std::optional<ores::utility::decimal::decimal> strike;

    /**
     * @brief Username of the person who last modified this equity target redemption forward.
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
    friend bool operator==(const equity_tarf_instrument&, const equity_tarf_instrument&) = default;
};

/**
 * @brief Dispatch-key identifier for equity_tarf_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_tarf_instrument&) {
    return "ores.trading.equity_tarf_instrument";
}

}

#endif
