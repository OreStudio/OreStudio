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
#include "ores.trading.api/domain/equity_cliquet_option.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

using ores::trading::domain::construct_equity_cliquet_option;
using ores::trading::domain::equity_cliquet_option_error;
using ores::trading::domain::equity_cliquet_option_terms;
using ores::utility::decimal::decimal;

equity_cliquet_option_terms make_terms() {
    equity_cliquet_option_terms terms;
    terms.underlying_name = ".SPX";
    terms.currency = "USD";
    terms.notional = *decimal::from_string("1000000");
    terms.option_type = "Call";
    terms.strike = *decimal::from_string("4500");
    terms.expiry_date = std::chrono::year{2027} / std::chrono::month{3} / std::chrono::day{15};
    terms.exercise_type = "European";
    terms.long_short = "Long";
    terms.settlement_type = "Cash";
    terms.frequency = "Quarterly";
    return terms;
}

}

using namespace ores::logging;

TEST_CASE("a_cliquet_is_constructed_as_the_cliquet_it_states", "[domain]") {
    auto lg(make_logger(test_suite));

    const auto sut = construct_equity_cliquet_option(make_terms());
    BOOST_LOG_SEV(lg, info) << "Cliquet underlying: " << sut.underlying_name;

    CHECK(sut.identity.trade_type_code == "EquityCliquetOption");
    CHECK(sut.cliquet_frequency == "Quarterly");
    CHECK(sut.underlying_name == ".SPX");
    CHECK(sut.option_type == "Call");
    CHECK(sut.notional.to_string() == "1000000");
    CHECK(sut.strike.to_string() == "4500");
    CHECK(sut.settlement_type == "Cash");
}

TEST_CASE("a_cliquet_without_a_frequency_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_terms();
    terms.frequency.clear();
    BOOST_LOG_SEV(lg, info) << "Refusing a cliquet with no frequency";

    CHECK_THROWS_AS(construct_equity_cliquet_option(terms), equity_cliquet_option_error);
}

TEST_CASE("a_cliquet_with_a_notional_that_is_not_positive_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_terms();
    terms.notional = decimal{};
    BOOST_LOG_SEV(lg, info) << "Refusing a cliquet with a zero notional";

    CHECK_THROWS_AS(construct_equity_cliquet_option(terms), equity_cliquet_option_error);
}

TEST_CASE("a_cliquet_with_an_option_type_outside_the_closed_set_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_terms();
    terms.option_type = "Straddle";
    BOOST_LOG_SEV(lg, info) << "Refusing a cliquet whose option type is not Call or Put";

    CHECK_THROWS_AS(construct_equity_cliquet_option(terms), equity_cliquet_option_error);
}

TEST_CASE("a_cliquet_with_an_exercise_type_outside_the_closed_set_is_refused", "[domain]") {
    auto lg(make_logger(test_suite));

    auto terms = make_terms();
    terms.exercise_type = "Perpetual";
    BOOST_LOG_SEV(lg, info) << "Refusing a cliquet whose exercise type is not ORE's";

    CHECK_THROWS_AS(construct_equity_cliquet_option(terms), equity_cliquet_option_error);
}
