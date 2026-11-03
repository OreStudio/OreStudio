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
#include "ores.ore.core/results/market_input_reader.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.testing/test_files.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>

namespace {

const std::string input_tags("[ore][results][market_input_reader]");

using ores::ore::results::family_named;
using ores::ore::results::is_market_input_family;
using ores::ore::results::read_market_inputs;

std::string sample_of(const std::string& code) {
    const auto* family = family_named(code);
    REQUIRE(family != nullptr);
    return ores::testing::read_file(ores::testing::project_root::resolve(family->sample_file));
}

}

TEST_CASE("a quote keeps its date, its datum and its value", input_tags) {
    const auto* family = family_named("market_data_report");
    REQUIRE(family != nullptr);
    CHECK(is_market_input_family(*family));

    const auto outcome = read_market_inputs("#datumDate,datumId,datumValue\n"
                                            "2026-05-05,FX/RATE/EUR/USD,1.1707000000\n",
                                            *family);

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 1);
    CHECK(outcome.rows[0].as_of == "2026-05-05");
    CHECK(outcome.rows[0].datum_id == "FX/RATE/EUR/USD");
    CHECK(outcome.rows[0].value == 1.1707);
    CHECK(outcome.rows[0].payment_date.empty());
    CHECK(outcome.rows[0].line == 2);
}

TEST_CASE("a dividend keeps its payment date", input_tags) {
    const auto* family = family_named("dividends_report");
    REQUIRE(family != nullptr);

    const auto outcome =
        read_market_inputs("#dividendExDate,equityId,dividendRate,dividendPaymentDate\n"
                           "2026-03-20,SP5,1.25,2026-04-01\n",
                           *family);

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 1);
    CHECK(outcome.rows[0].datum_id == "SP5");
    CHECK(outcome.rows[0].value == 1.25);
    CHECK(outcome.rows[0].payment_date == "2026-04-01");
}

TEST_CASE("a fixing whose value is not a figure is refused by line", input_tags) {
    const auto* family = family_named("fixings_report");
    REQUIRE(family != nullptr);

    const auto outcome =
        read_market_inputs("#fixingDate,fixingId,fixingValue\n2026-01-28,EQ-SP5,soon\n", *family);

    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals.front().find("line 2") != std::string::npos);
    CHECK(outcome.rows.empty());
}

TEST_CASE("a family that is not a market input is refused", input_tags) {
    const auto* family = family_named("npv_report");
    REQUIRE(family != nullptr);
    CHECK_FALSE(is_market_input_family(*family));
    CHECK_FALSE(read_market_inputs("#TradeId,NPV\n", *family).refusals.empty());
}

TEST_CASE("the checked-in market input samples read whole", input_tags) {
    const auto quotes = read_market_inputs(sample_of("market_data_report"), *family_named("market_data_report"));
    REQUIRE(quotes.refusals.empty());
    CHECK(quotes.rows.size() == 830);

    const auto fixings = read_market_inputs(sample_of("fixings_report"), *family_named("fixings_report"));
    REQUIRE(fixings.refusals.empty());
    CHECK(fixings.rows.size() == 5452);

    const auto dividends = read_market_inputs(sample_of("dividends_report"), *family_named("dividends_report"));
    CHECK(dividends.refusals.empty());
    CHECK(dividends.rows.empty());
}
