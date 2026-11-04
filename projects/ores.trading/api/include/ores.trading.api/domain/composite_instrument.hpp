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
#ifndef ORES_TRADING_API_DOMAIN_COMPOSITE_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_COMPOSITE_INSTRUMENT_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/instrument_identity.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <type_traits>

namespace ores::trading::domain {

/**
 * @brief Composite instrument economics for CompositeTrade, TotalReturnSwap and
 * ContractForDifference.
 *
 * Represents the composite product types ORE states. trade_type_code
 * discriminates the exact product.
 *
 * A composite trade's components are trades of their own, joined to the
 * composite by a structure of the CompositeTrade template: the composite
 * holds the composite role and each component the trade or sub_trade
 * role it held in the document. This row keeps the composite's own terms:
 * its currency, how its notional is formed, the basket's name and index
 * quantity. The table is bi-temporal and audited, so the model takes the
 * ordinary audited shape and needs no shape flag.
 */
struct composite_instrument final {
    instrument_identity identity;

    /**
     * @brief Optional free-text description.
     */
    std::string description;

    /**
     * @brief The currency a composite trade is priced in, as Currency states it. Null for a total
     * return swap or a contract for difference, which state no such field.
     */
    std::optional<std::string> currency;

    /**
     * @brief How the composite's notional is formed from its components', as NotionalCalculation
     * states it: Sum, Mean, First, Last, Min, Max or Override.
     */
    std::optional<std::string> notional_calculation;

    /**
     * @brief The notional the composite states outright, as NotionalOverride states it, when the
     * calculation is Override.
     */
    std::optional<ores::utility::decimal::decimal> notional_override;

    /**
     * @brief Whether the composite is a portfolio basket, as PortfolioBasket states it.
     */
    std::optional<std::string> portfolio_basket;

    /**
     * @brief The quantity of a portfolio basket's index, as IndexQuantity states it.
     */
    std::optional<ores::utility::decimal::decimal> index_quantity;

    /**
     * @brief Whether the trade pays the underlying's return, as ReturnData/Payer states it.
     */
    std::optional<std::string> return_payer;

    /**
     * @brief The currency the return is paid in, as ReturnData/Currency states it.
     */
    std::optional<std::string> return_currency;

    /**
     * @brief How far before each period the return is observed, as ReturnData/ObservationLag states
     * it.
     */
    std::optional<std::string> return_observation_lag;

    /**
     * @brief The business day convention of the observations, as ReturnData/ObservationConvention
     * states it.
     */
    std::optional<std::string> return_observation_convention;

    /**
     * @brief The calendar the observations are adjusted on, as ReturnData/ObservationCalendar
     * states it.
     */
    std::optional<std::string> return_observation_calendar;

    /**
     * @brief How far after each period the return is paid, as ReturnData/PaymentLag states it.
     */
    std::optional<std::string> return_payment_lag;

    /**
     * @brief The business day convention of the return payments, as ReturnData/PaymentConvention
     * states it.
     */
    std::optional<std::string> return_payment_convention;

    /**
     * @brief The calendar the return payments are adjusted on, as ReturnData/PaymentCalendar states
     * it.
     */
    std::optional<std::string> return_payment_calendar;

    /**
     * @brief The underlying's price at the start, as ReturnData/InitialPrice states it.
     */
    std::optional<ores::utility::decimal::decimal> return_initial_price;

    /**
     * @brief The currency of the initial price, as ReturnData/InitialPriceCurrency states it.
     */
    std::optional<std::string> return_initial_price_currency;

    /**
     * @brief The FX indices that convert the underlyings' currencies into the return's, as
     * ReturnData/FXTerms/FXIndex states them, in order and separated by spaces.
     */
    std::optional<std::string> return_fx_index;

    /**
     * @brief Whether the underlying's cash flows are paid when they occur, as
     * ReturnData/PayUnderlyingCashFlowsImmediately states it.
     */
    std::optional<std::string> return_pay_underlying_cash_flows_immediately;

    /**
     * @brief The notional type of each funding leg, as FundingData/NotionalType states them, in
     * order and separated by spaces.
     */
    std::optional<std::string> funding_notional_types;

    /**
     * @brief The name of the portfolio index the trade pays the return of, as
     * UnderlyingData/PortfolioIndexTradeData/BasketName states it.
     */
    std::optional<std::string> underlying_basket_name;

    /**
     * @brief The quantity of that index, as PortfolioIndexTradeData/IndexQuantity states it.
     */
    std::optional<std::string> underlying_index_quantity;

    /**
     * @brief Whether the index's price is per unit, as PortfolioIndexTradeData/PriceIsPerUnit
     * states it.
     */
    std::optional<std::string> underlying_price_is_per_unit;

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
    friend bool operator==(const composite_instrument&, const composite_instrument&) = default;
};

/**
 * @brief The members of composite_instrument the customer agreed to.
 *
 * Declared by the model's :economic: property on each column.
 */
inline constexpr std::string_view composite_instrument_economic_fields[] = {
    "currency",
    "notional_calculation",
    "notional_override",
    "portfolio_basket",
    "index_quantity",
    "return_payer",
    "return_currency",
    "return_observation_lag",
    "return_observation_convention",
    "return_observation_calendar",
    "return_payment_lag",
    "return_payment_convention",
    "return_payment_calendar",
    "return_initial_price",
    "return_initial_price_currency",
    "return_fx_index",
    "return_pay_underlying_cash_flows_immediately",
    "funding_notional_types",
    "underlying_basket_name",
    "underlying_index_quantity",
    "underlying_price_is_per_unit",
};

/**
 * @brief Names the members of composite_instrument that are economic.
 *
 * The economic digest reads this at compile time and decides nothing by a
 * field's name. A type without this overload cannot be digested.
 */
[[nodiscard]] constexpr std::span<const std::string_view>
economic_fields_of(std::type_identity<composite_instrument>) {
    return composite_instrument_economic_fields;
}

/**
 * @brief Dispatch-key identifier for composite_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const composite_instrument&) {
    return "ores.trading.composite_instrument";
}

}

#endif
