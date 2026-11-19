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
#ifndef ORES_ORE_CORE_DOMAIN_VOLATILITY_PRODUCT_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_VOLATILITY_PRODUCT_PROJECTION_HPP

#include "ores.ore.core/domain/decimal_projection.hpp"
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/scalar_terms_projection.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

/**
 * @file volatility_product_projection.hpp
 * @brief The elements ORE's equity and FX basket variance swaps and forward
 * volatility agreements state.
 *
 * Each element has one shape in both families, so one read and one write serve
 * the two families' rows. The row keeps the element's scalars; the members, the
 * underlying and the valuation schedule are the carrier's shared rows, which the
 * families' tables read and write.
 */

namespace ores::ore::domain::volatility_product {

using ores::utility::decimal::decimal;

inline std::optional<decimal> read_decimal(const xml::record_reader& data,
                                           std::string_view element) {
    if (const auto text = scalar_terms::read_text(data, element))
        if (const auto value = decimal::from_string(*text))
            return *value;
    return std::nullopt;
}

inline std::chrono::year_month_day read_date(const xml::record_reader& data,
                                             std::string_view element) {
    if (const auto text = scalar_terms::read_text(data, element))
        return option_block::to_optional_domain_date(*text).value_or(std::chrono::year_month_day{});
    return {};
}

inline void write_decimal(xml::record_writer& data,
                          std::string_view element,
                          const std::optional<decimal>& value) {
    if (value)
        scalar_terms::write_text(data, element, value->to_string());
}

inline void write_date(xml::record_writer& data,
                       std::string_view element,
                       const std::chrono::year_month_day& value) {
    scalar_terms::write_text(data, element, option_block::to_ore_date(value));
}

/**
 * @brief Reads a basket variance swap's scalars onto the row.
 *
 * The schema requires every scalar but the cap and the floor, so the document
 * reaches the reader with them stated.
 */
template <typename Row>
void read_basket_variance_swap(const xml::record_reader& data, Row& row) {
    row.long_short = scalar_terms::read_text(data, "LongShort").value_or("");
    row.strike = read_decimal(data, "Strike").value_or(decimal{});
    row.notional = read_decimal(data, "Notional").value_or(decimal{});
    const auto squared = scalar_terms::read_text(data, "SquaredPayoff").value_or("");
    row.squared_payoff = squared == "true" || squared == "1";
    row.cap = read_decimal(data, "Cap");
    row.floor = read_decimal(data, "Floor");
    row.settlement_date = read_date(data, "SettlementDate");
    row.currency = scalar_terms::read_text(data, "Currency").value_or("");
}

/// Writes a basket variance swap's scalars into the element.
template <typename Row>
void write_basket_variance_swap(xml::record_writer& data, const Row& row) {
    scalar_terms::write_text(data, "LongShort", row.long_short);
    write_decimal(data, "Strike", row.strike);
    write_decimal(data, "Notional", row.notional);
    scalar_terms::write_text(data, "SquaredPayoff", row.squared_payoff ? "true" : "false");
    write_decimal(data, "Cap", row.cap);
    write_decimal(data, "Floor", row.floor);
    write_date(data, "SettlementDate", row.settlement_date);
    scalar_terms::write_text(data, "Currency", row.currency);
}

/**
 * @brief Reads a forward volatility agreement's scalars onto the row.
 *
 * The schema requires the dates, the position, the strike, the quantity and the
 * currency; the other terms are optional.
 */
template <typename Row>
void read_forward_volatility_agreement(const xml::record_reader& data, Row& row) {
    row.fva_date = read_date(data, "FvaDate");
    row.option_expiry = read_date(data, "OptionExpiry");
    row.premium_date = read_date(data, "PremiumDate");
    row.long_short = scalar_terms::read_text(data, "LongShort").value_or("");
    row.implied_vol_strike = decimal_as_double(data, "ImpliedVolStrike").value_or(0.0);
    row.quantity = decimal_as_double(data, "Quantity").value_or(0.0);
    row.pay_currency = scalar_terms::read_text(data, "PayCcy").value_or("");
    row.settlement_date = read_date(data, "SettlementDate");
    row.underlying_strike = read_decimal(data, "UnderlyingStrike");
    row.day_count_fraction = scalar_terms::read_text(data, "DayCountFraction");
    row.dividend_yield = read_decimal(data, "DividendYield");
    row.fixed_rate = read_decimal(data, "FixedRate");
    row.payoff_type = scalar_terms::read_text(data, "PayoffType");
}

/// Writes a forward volatility agreement's scalars into the element.
template <typename Row>
void write_forward_volatility_agreement(xml::record_writer& data, const Row& row) {
    write_date(data, "FvaDate", row.fva_date);
    write_date(data, "OptionExpiry", row.option_expiry);
    write_date(data, "PremiumDate", row.premium_date);
    scalar_terms::write_text(data, "LongShort", row.long_short);
    set_decimal_from_double(data, "ImpliedVolStrike", row.implied_vol_strike);
    set_decimal_from_double(data, "Quantity", row.quantity);
    scalar_terms::write_text(data, "PayCcy", row.pay_currency);
    write_date(data, "SettlementDate", row.settlement_date);
    write_decimal(data, "UnderlyingStrike", row.underlying_strike);
    if (row.day_count_fraction)
        scalar_terms::write_text(data, "DayCountFraction", *row.day_count_fraction);
    write_decimal(data, "DividendYield", row.dividend_yield);
    write_decimal(data, "FixedRate", row.fixed_rate);
    if (row.payoff_type)
        scalar_terms::write_text(data, "PayoffType", *row.payoff_type);
}

}

#endif
