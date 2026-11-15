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
#include "ores.ore.core/domain/conventions_package.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <utility>
#include <vector>

namespace {

const std::string conventions_fx_test_suite("ores::ore::core::tests");
const std::string conventions_fx_tags("[ore][domain][conventions][fx]");

using ores::ore::domain::fx_conventions;
using ores::ore::domain::fx_pair_source;

fx_pair_source source_of_pair(const std::string& base,
                              const std::string& quote,
                              const std::string& pair_code,
                              double pip_factor,
                              int spot_days,
                              std::vector<std::string> calendars) {
    fx_pair_source source;
    source.pair_code = pair_code;
    source.base_currency = base;
    source.quote_currency = quote;
    source.spot_days = spot_days;
    source.convention.pair_code = pair_code;
    source.convention.pip_factor = pip_factor;
    source.convention.tick_size = 0.1;
    source.convention.decimal_places = 4;
    source.convention.business_day_convention = "ModifiedFollowing";
    source.convention.spot_relative = true;
    source.convention.end_of_month = false;
    source.advance_calendars = std::move(calendars);
    return source;
}

}

TEST_CASE("every_pair_the_store_holds_becomes_one_fx_convention", conventions_fx_tags) {
    auto lg(ores::logging::make_logger(conventions_fx_test_suite));

    const std::vector<fx_pair_source> sources{
        source_of_pair("AUD", "USD", "AUD/USD", 0.0001, 2, {"US", "AU"}),
        source_of_pair("USD", "JPY", "USD/JPY", 0.01, 2, {"JP", "US"}),
    };

    const auto written = fx_conventions(sources, {});

    REQUIRE(written.size() == 2);

    CHECK(written[0].pair.base_currency == "AUD");
    CHECK(written[0].pair.quote_currency == "USD");
    CHECK(written[0].pair.pair_code == "AUD/USD");
    CHECK(written[0].spot_days == 2);
    CHECK(written[0].convention.pip_factor == 0.0001);
    CHECK(written[0].convention.tick_size == 0.1);
    CHECK(written[0].convention.decimal_places == 4);
    CHECK(written[0].convention.business_day_convention == "ModifiedFollowing");
    CHECK(written[0].convention.spot_relative == true);
    CHECK(written[0].convention.end_of_month == false);
    CHECK(written[0].advance_calendars == std::vector<std::string>{"US", "AU"});

    CHECK(written[1].pair.base_currency == "USD");
    CHECK(written[1].pair.quote_currency == "JPY");
    CHECK(written[1].convention.pip_factor == 0.01);
    CHECK(written[1].spot_days == 2);
}

TEST_CASE("a_name_an_option_convention_states_is_written_with_the_pair_it_names",
          conventions_fx_tags) {
    auto lg(ores::logging::make_logger(conventions_fx_test_suite));

    const std::vector<fx_pair_source> sources{
        source_of_pair("AUD", "USD", "AUD/USD", 0.0001, 2, {}),
        source_of_pair("USD", "JPY", "USD/JPY", 0.01, 2, {}),
    };
    const std::vector<std::string> references{
        "AUD-USD-FX",
        "USD-JPY-FX",
        "FX-ECB-EUR-USD",
    };

    const auto written = fx_conventions(sources, references);

    REQUIRE(written.size() == 2);
    CHECK(written[0].id == "AUD-USD-FX");
    CHECK(written[0].pair.base_currency == "AUD");
    CHECK(written[0].pair.quote_currency == "USD");
    CHECK(written[1].id == "USD-JPY-FX");
    CHECK(written[1].pair.base_currency == "USD");
    CHECK(written[1].pair.quote_currency == "JPY");
}

TEST_CASE("a_pair_no_option_convention_names_keeps_the_canonical_spelling", conventions_fx_tags) {
    auto lg(ores::logging::make_logger(conventions_fx_test_suite));

    const std::vector<fx_pair_source> sources{
        source_of_pair("AUD", "USD", "AUD/USD", 0.0001, 2, {}),
        source_of_pair("NZD", "USD", "NZD/USD", 0.0001, 2, {}),
    };
    const std::vector<std::string> references{"AUD-USD-FX"};

    const auto written = fx_conventions(sources, references);

    REQUIRE(written.size() == 2);
    CHECK(written[0].id == "AUD-USD-FX");
    CHECK(written[1].id.empty());
    CHECK(written[1].pair.base_currency == "NZD");
    CHECK(written[1].pair.quote_currency == "USD");
}

TEST_CASE("a_name_that_quotes_the_pair_the_other_way_round_states_its_own_currencies",
          conventions_fx_tags) {
    auto lg(ores::logging::make_logger(conventions_fx_test_suite));

    // refdata holds one row per pair, and a document may name either direction.
    const std::vector<fx_pair_source> sources{
        source_of_pair("EUR", "USD", "EUR/USD", 0.0001, 2, {}),
    };
    const std::vector<std::string> references{"USD-EUR-FX"};

    const auto written = fx_conventions(sources, references);

    REQUIRE(written.size() == 1);
    CHECK(written[0].id == "USD-EUR-FX");
    CHECK(written[0].pair.base_currency == "USD");
    CHECK(written[0].pair.quote_currency == "EUR");
    CHECK(written[0].convention.pip_factor == 0.0001);
}

TEST_CASE("a_pair_named_twice_is_written_under_both_names", conventions_fx_tags) {
    auto lg(ores::logging::make_logger(conventions_fx_test_suite));

    const std::vector<fx_pair_source> sources{
        source_of_pair("GBP", "USD", "GBP/USD", 0.0001, 2, {}),
    };
    const std::vector<std::string> references{"GBP-USD-FX", "USD-GBP-FX", "GBP-USD-FX"};

    const auto written = fx_conventions(sources, references);

    REQUIRE(written.size() == 2);
    CHECK(written[0].id == "GBP-USD-FX");
    CHECK(written[0].pair.base_currency == "GBP");
    CHECK(written[1].id == "USD-GBP-FX");
    CHECK(written[1].pair.base_currency == "USD");
}

TEST_CASE("a_pair_missing_a_leg_is_not_written", conventions_fx_tags) {
    auto lg(ores::logging::make_logger(conventions_fx_test_suite));

    const std::vector<fx_pair_source> sources{
        source_of_pair("AUD", "USD", "AUD/USD", 0.0001, 2, {}),
        source_of_pair("AUD", "", "AUD/", 0.0001, 2, {}),
    };

    const auto written = fx_conventions(sources, {});

    REQUIRE(written.size() == 1);
    CHECK(written[0].pair.quote_currency == "USD");
}
