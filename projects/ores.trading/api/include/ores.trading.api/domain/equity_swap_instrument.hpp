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
#ifndef ORES_TRADING_API_DOMAIN_EQUITY_SWAP_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_EQUITY_SWAP_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>

namespace ores::trading::domain {

/**
 * @brief Equity Swap instrument.
 *
 * Routes ORE product types: EquitySwap, EquityWorstOfBasketSwap. For
 * basket swaps, underlying_name is NULL and the basket is the constituent rows
 * that name the trade; for single-name swaps, the constituents are absent.
 */
struct equity_swap_instrument final {
    instrument_identity identity;

    /**
     * @brief Single underlying; empty for basket swaps.
     */
    std::string underlying_name;

    /**
     * @brief ISO 4217 currency code.
     *
     * Soft FK to ores_refdata_currencies_tbl: ISO 4217 currency codes belong to ores.refdata, so
     * the dependency is recorded rather than copied. PR 4 tightens the soft reference into a real
     * foreign key.
     */
    std::string currency;

    /**
     * @brief Must be positive.
     */
    ores::utility::decimal::decimal notional;

    /**
     * @brief Total or Price.
     *
     * Soft FK to ores_trading_return_types_tbl: ORE states EquityLegData.ReturnType as a bare
     * xs:string, so the set is the one the ORE example corpus uses (Total, Price). PR 4 tightens
     * the soft reference into a real foreign key.
     */
    std::string return_type;

    /**
     * @brief ISO 8601 date.
     */
    std::chrono::year_month_day start_date;

    /**
     * @brief ISO 8601 date.
     */
    std::chrono::year_month_day maturity_date;

    /**
     * @brief Long or Short.
     */
    std::string long_short;

    /**
     * @brief e.g. 3M, 6M, 1Y.
     *
     * Soft FK to ores_refdata_payment_frequencies_tbl: payment frequencies belong to ores.refdata,
     * so the dependency is recorded rather than copied. PR 4 tightens the soft reference into a
     * real foreign key.
     */
    std::string payment_frequency_code;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    ores::dq::domain::audit_record audit;
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
    friend bool operator==(const equity_swap_instrument&, const equity_swap_instrument&) = default;
};

/**
 * @brief The members of equity_swap_instrument the customer agreed to.
 *
 * Declared by the model's :economic: property on each column.
 */
inline constexpr std::string_view equity_swap_instrument_economic_fields[] = {
    "underlying_name",
    "currency",
    "notional",
    "return_type",
    "start_date",
    "maturity_date",
    "long_short",
    "payment_frequency_code",
};

/**
 * @brief Names the members of equity_swap_instrument that are economic.
 *
 * The economic digest reads this at compile time and decides nothing by a
 * field's name. A type without this overload cannot be digested.
 */
[[nodiscard]] constexpr std::span<const std::string_view>
economic_fields_of(std::type_identity<equity_swap_instrument>) {
    return equity_swap_instrument_economic_fields;
}

/**
 * @brief Dispatch-key identifier for equity_swap_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const equity_swap_instrument&) {
    return "ores.trading.equity_swap_instrument";
}

}

#endif
