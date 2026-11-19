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
#ifndef ORES_TRADING_API_DOMAIN_CBO_STRUCTURE_HPP
#define ORES_TRADING_API_DOMAIN_CBO_STRUCTURE_HPP

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
 * The structure a CBO states inline: the scalars of its CBOStructure.
 *
 * A CBO names its structure by its StructureId, which ORE looks up in its
 * reference data, or states it inline. An inline structure's tranches, basket
 * and payment schedule are rows of their own; this row keeps the scalars.
 */
struct cbo_structure final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The CBO this row belongs to.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief The day counter of the notes, as DayCounter states it.
     */
    std::string day_counter;

    /**
     * @brief The payment convention, as PaymentConvention states it.
     */
    std::string payment_convention;

    /**
     * @brief The currency of the notes, as Currency states it.
     */
    std::string currency;

    /**
     * @brief The end of the reinvestment period, as ReinvestmentEndDate states it.
     */
    std::optional<std::chrono::year_month_day> reinvestment_end_date;

    /**
     * @brief The senior fee rate, as SeniorFee states it.
     */
    ores::utility::decimal::decimal senior_fee;

    /**
     * @brief The day counter of the fees, as FeeDayCounter states it.
     */
    std::string fee_day_counter;

    /**
     * @brief The subordinated fee rate, as SubordinatedFee states it.
     */
    ores::utility::decimal::decimal subordinated_fee;

    /**
     * @brief The share of the residual the senior fee takes, as EquityKicker states it.
     */
    ores::utility::decimal::decimal equity_kicker;

    /**
     * @brief Username of the person who last modified this cbo structure.
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
    friend bool operator==(const cbo_structure&, const cbo_structure&) = default;
};

/**
 * @brief Dispatch-key identifier for cbo_structure, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const cbo_structure&) {
    return "ores.trading.cbo_structure";
}

}

#endif
