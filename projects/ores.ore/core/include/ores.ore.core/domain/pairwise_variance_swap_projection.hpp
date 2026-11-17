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
#ifndef ORES_ORE_CORE_DOMAIN_PAIRWISE_VARIANCE_SWAP_PROJECTION_HPP
#define ORES_ORE_CORE_DOMAIN_PAIRWISE_VARIANCE_SWAP_PROJECTION_HPP

#include "ores.ore.core/domain/index_reference.hpp"
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <charconv>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file pairwise_variance_swap_projection.hpp
 * @brief The element ORE's equity and FX pairwise variance swaps share.
 *
 * Both products state the same element, so both rows keep the same members and
 * one read and one write serve the two families. ORE requires exactly two
 * underlyings, so each member's index, strike and notional is a column of the
 * row; the valuation schedules are the carrier's schedule rows.
 */

namespace ores::ore::domain::pairwise_variance_swap {

using ores::utility::decimal::decimal;

namespace detail {

inline auto decimal_of(const std::string& text) -> decimal {
    if (const auto parsed = decimal::from_string(text))
        return *parsed;
    return decimal{};
}

inline auto values_of(const xml::record_reader& data, std::string_view list)
    -> std::vector<std::string> {
    if (const auto node = data.child(list))
        return node->values("Value");
    return {};
}

/// A free-style scalar is a node whose own text is the value.
inline auto free_text(const xml::record_reader& data, std::string_view element)
    -> std::optional<std::string> {
    if (const auto node = data.child(element))
        return node->own_text();
    return std::nullopt;
}

inline void
set_free_text(xml::record_writer& data, std::string_view element, const std::string& value) {
    data.child(element).set_own_text(value);
}

inline auto optional_decimal(const xml::record_reader& data, std::string_view element)
    -> std::optional<decimal> {
    if (const auto text = free_text(data, element))
        if (const auto parsed = decimal::from_string(*text))
            return *parsed;
    return std::nullopt;
}

inline auto decimals_of(const xml::record_reader& data, std::string_view list)
    -> std::vector<decimal> {
    if (const auto node = data.child(list))
        return node->decimals("Value");
    return {};
}

inline void append_decimals(xml::record_writer& data,
                            std::string_view list,
                            const decimal& first,
                            const decimal& second) {
    auto node = data.child(list);
    node.append_decimal("Value", first);
    node.append_decimal("Value", second);
}

}

/**
 * @brief Reads the element's terms into the row.
 *
 * ORE refuses an element whose lists do not hold exactly two members, or whose
 * settlement date or accrual lag does not parse, so the read refuses it too
 * rather than keep a value the document did not state.
 */
template <typename Row>
void read(const xml::record_reader& data, Row& r) {
    r.long_short = detail::free_text(data, "LongShort").value_or("");
    const auto underlyings = detail::values_of(data, "Underlyings");
    const auto strikes = detail::decimals_of(data, "UnderlyingStrikes");
    const auto notionals = detail::decimals_of(data, "UnderlyingNotionals");
    if (underlyings.size() != 2 || strikes.size() != 2 || notionals.size() != 2)
        throw std::invalid_argument(
            "A pairwise variance swap states exactly two underlyings, strikes and notionals.");
    r.first_underlying = address_of(underlyings[0]);
    r.second_underlying = address_of(underlyings[1]);
    r.first_strike = strikes[0];
    r.second_strike = strikes[1];
    r.first_notional = notionals[0];
    r.second_notional = notionals[1];
    r.basket_notional = detail::decimal_of(detail::free_text(data, "BasketNotional").value_or(""));
    r.basket_strike = detail::decimal_of(detail::free_text(data, "BasketStrike").value_or(""));
    r.currency = detail::free_text(data, "PayCcy").value_or("");
    const auto settlement = option_block::to_optional_domain_date(
        detail::free_text(data, "SettlementDate").value_or(""));
    if (!settlement)
        throw std::invalid_argument("A pairwise variance swap states a settlement date.");
    r.settlement_date = *settlement;
    if (const auto lag = detail::free_text(data, "AccrualLag")) {
        int value = 0;
        const auto [end, error] = std::from_chars(lag->data(), lag->data() + lag->size(), value);
        if (error != std::errc{} || end != lag->data() + lag->size())
            throw std::invalid_argument(
                "A pairwise variance swap's accrual lag is a whole number.");
        r.accrual_lag = value;
    }
    r.payoff_limit = detail::optional_decimal(data, "PayoffLimit");
    r.cap = detail::optional_decimal(data, "Cap");
    r.floor = detail::optional_decimal(data, "Floor");
}

/// Writes the row's terms on the element; the caller writes the schedules.
template <typename Row>
void write(xml::record_writer& data, const Row& v) {
    detail::set_free_text(data, "LongShort", v.long_short);
    auto underlyings = data.child("Underlyings");
    underlyings.append_value("Value", ore_name_of(v.first_underlying));
    underlyings.append_value("Value", ore_name_of(v.second_underlying));
    detail::append_decimals(data, "UnderlyingStrikes", v.first_strike, v.second_strike);
    detail::append_decimals(data, "UnderlyingNotionals", v.first_notional, v.second_notional);
    detail::set_free_text(data, "BasketNotional", v.basket_notional.to_string());
    detail::set_free_text(data, "BasketStrike", v.basket_strike.to_string());
    detail::set_free_text(data, "SettlementDate", option_block::to_ore_date(v.settlement_date));
    detail::set_free_text(data, "PayCcy", v.currency);
    if (v.accrual_lag)
        detail::set_free_text(data, "AccrualLag", std::to_string(*v.accrual_lag));
    for (const auto& [element, value] : {std::pair{"PayoffLimit", &v.payoff_limit},
                                         std::pair{"Cap", &v.cap},
                                         std::pair{"Floor", &v.floor}})
        if (*value)
            detail::set_free_text(data, element, (*value)->to_string());
}

}

#endif
