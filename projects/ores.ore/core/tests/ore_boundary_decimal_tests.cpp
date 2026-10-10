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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/ore_boundary_decimal.hpp"
#include "ores.ore.core/domain/swap_instrument_mapper.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <catch2/catch_test_macros.hpp>
#include <limits>
#include <string>

/**
 * @file ore_boundary_decimal_tests.cpp
 * @brief The ORE boundary hands a money column the digits the document wrote.
 *
 * An ORE number is an xs:float, so the parsed float alone carries only the
 * binary value nearest the document's text. The binding keeps the text beside
 * the float, and the mapper reads the exact decimal from that text. These
 * tests pin that down: the same document read through the float would give a
 * different decimal, so the assertions below fail if the text is dropped.
 */

namespace {

const std::string_view test_suite("ores.ore.boundary.decimal.tests");
const std::string tags("[ore][xml][mapper][boundary][decimal]");

using ores::ore::domain::swap_instrument_mapper;
using ores::utility::decimal::decimal;
using namespace ores::logging;

std::string swap_with_fixed_rate(const std::string& rate_text) {
    return "<Portfolio>\n"
           "  <Trade id=\"LexicalRate\">\n"
           "    <TradeType>Swap</TradeType>\n"
           "    <SwapData>\n"
           "      <LegData>\n"
           "        <Payer>false</Payer>\n"
           "        <LegType>Fixed</LegType>\n"
           "        <Currency>EUR</Currency>\n"
           "        <Notionals><Notional>1000000</Notional></Notionals>\n"
           "        <FixedLegData><Rates><Rate>" +
           rate_text +
           "</Rate></Rates></FixedLegData>\n"
           "      </LegData>\n"
           "    </SwapData>\n"
           "  </Trade>\n"
           "</Portfolio>\n";
}

ores::ore::domain::trade load_first_trade(const std::string& content) {
    ores::ore::domain::portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    return p.Trade.front();
}

decimal fixed_rate_of(const ores::trading::domain::swap_instrument_data& data) {
    for (const auto& r : data.leg_rates)
        if (r.rate_role == "fixed")
            return r.value;
    return decimal{};
}

}

TEST_CASE("the mapper keeps the document's float digits, not the nearest float", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade(swap_with_fixed_rate("0.00999999978"));

    // The binding itself carries the text the document wrote.
    const auto& binding = t.SwapData->LegData.front().legDataType->FixedLegData->Rates.Rate.front();
    CHECK(binding.lexical() == "0.00999999978");

    const auto mapped = fixed_rate_of(swap_instrument_mapper::forward_swap(t));

    const auto via_digits = decimal::from_string("0.00999999978").value();
    const auto via_float = decimal::from_double(static_cast<double>(0.00999999978f)).value();
    // The two disagree, so a mapper that had thrown the text away would not
    // satisfy the assertion below.
    CHECK(via_digits != via_float);
    CHECK(mapped == via_digits);
    CHECK(mapped != via_float);
    BOOST_LOG_SEV(lg, info) << "Lexical rate test passed";
}

TEST_CASE("a rate with more significant digits than a float holds survives intact", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade(swap_with_fixed_rate("0.123456789"));

    const auto mapped = fixed_rate_of(swap_instrument_mapper::forward_swap(t));

    CHECK(mapped == decimal::from_string("0.123456789").value());
    CHECK(mapped != decimal::from_double(static_cast<double>(0.123456789f)).value());
    BOOST_LOG_SEV(lg, info) << "Significant-digit rate test passed";
}

TEST_CASE("a binding set in code falls back to its value and a computed double too", tags) {
    xsd::base<float> programmatic;
    programmatic = 0.5f;
    CHECK(programmatic.lexical().empty());
    CHECK(ores::ore::domain::exact_decimal(programmatic) == decimal::from_string("0.5").value());

    CHECK(ores::ore::domain::exact_decimal(0.25) == decimal::from_string("0.25").value());
}

TEST_CASE("exact_decimal falls back for INF and NaN instead of throwing", tags) {
    xsd::base<float> infinite;
    infinite.lexical("INF");
    infinite = std::numeric_limits<float>::infinity();
    CHECK_NOTHROW(ores::ore::domain::exact_decimal(infinite));
    CHECK(ores::ore::domain::exact_decimal(infinite) == decimal{});

    xsd::base<double> not_a_number;
    not_a_number.lexical("NaN");
    not_a_number = std::numeric_limits<double>::quiet_NaN();
    CHECK_NOTHROW(ores::ore::domain::exact_decimal(not_a_number));
    CHECK(ores::ore::domain::exact_decimal(not_a_number) == decimal{});
}
