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
#include "ores.marketdata.core/oresmd/pillar_quote_key.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <string>

/**
 * @file oresmd_pillar_quote_key_tests.cpp
 * @brief Pins the ORE key a curve pillar's quote carries and the series identity
 * that key projects to.
 *
 * A pillar of a short-end curve is the meeting-dated OIS quote that starts it. The
 * feed publishes one such key per pillar and the bootstrap derives the same key from
 * its own pillar list, so this is the one place both sides get their spelling from:
 * a change here moves the writer and the reader together, and the expected values are
 * spelled out rather than recomputed, so a change that moves only one of them fails.
 *
 * A curve template names the tenor its pillar matures at, and a curve configuration
 * names the same pillar by that tenor, so a named pillar is keyed by the tenor and
 * not by the days its end date happens to lie beyond spot: the two dates a run picks
 * move with the valuation date, the tenor a configuration wrote down does not.
 *
 * The end date is the observation's point, so it leaves the identity. That is what
 * lets the reader ask for a series by its key without already knowing which of the
 * series' points it wants.
 */

namespace {

using ores::marketdata::core::make_pillar_quote_key;
using ores::marketdata::core::pillar_datum_uri;
using ores::marketdata::core::pillar_series_uri;

constexpr auto spot_start = std::chrono::year{2026} / 1 / 28;
constexpr auto first_end = std::chrono::year{2026} / 3 / 19;
constexpr auto second_end = std::chrono::year{2026} / 4 / 29;

}

TEST_CASE("a_pillar_that_starts_at_spot_keys_0d_and_the_days_to_its_end", "[oresmd][pillar]") {
    // ORE reads a swap's start and end as two periods or two dates, so a spot
    // pillar's end is the days it lies after spot: 2026-01-28 to 2026-03-19 is 50.
    const auto key = make_pillar_quote_key("USD", "1D", "SPOT", spot_start, first_end);

    CHECK(key.series_type == "IR_SWAP");
    CHECK(key.metric == "RATE");
    CHECK(key.qualifier == "USD/0D/1D");
    CHECK(key.point == "50D");
    CHECK(pillar_series_uri(key) ==
          "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D");
    CHECK(pillar_datum_uri(key) ==
          "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D"
          "&term=50D");
}

TEST_CASE("a_pillar_that_starts_on_a_meeting_keys_the_date_it_starts_on", "[oresmd][pillar]") {
    const auto key = make_pillar_quote_key("USD", "1D", "1F", spot_start, first_end);

    CHECK(key.qualifier == "USD/20260128/1D");
    CHECK(key.point == "20260319");
    CHECK(pillar_series_uri(key) ==
          "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=20260128&tenor=1D");
}

TEST_CASE("a_pillars_end_date_is_its_point_and_not_part_of_its_series", "[oresmd][pillar]") {
    const auto short_pillar = make_pillar_quote_key("USD", "1D", "1F", spot_start, first_end);
    const auto long_pillar = make_pillar_quote_key("USD", "1D", "1F", spot_start, second_end);

    CHECK(short_pillar.point != long_pillar.point);
    CHECK(pillar_series_uri(short_pillar) == pillar_series_uri(long_pillar));
}

TEST_CASE("two_pillars_are_two_series", "[oresmd][pillar]") {
    const auto spot = make_pillar_quote_key("USD", "1D", "SPOT", spot_start, first_end);
    const auto dated =
        make_pillar_quote_key("USD", "1D", "1F", std::chrono::year{2026} / 1 / 28, second_end);

    CHECK(pillar_series_uri(spot) != pillar_series_uri(dated));
    CHECK(pillar_series_uri(spot) ==
          "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D");
    CHECK(pillar_series_uri(dated) ==
          "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=20260128&tenor=1D");
}

TEST_CASE("a_spot_pillar_that_does_not_end_after_spot_is_refused", "[oresmd][pillar]") {
    CHECK_THROWS_AS(make_pillar_quote_key("USD", "1D", "SPOT", spot_start, spot_start),
                    std::invalid_argument);
    CHECK_THROWS_AS(
        make_pillar_quote_key("USD", "1D", "SPOT", spot_start, std::chrono::year{2026} / 1 / 25),
        std::invalid_argument);
}

TEST_CASE("the index tenor is the tenor field of the series, so two index tenors are two series",
          "[oresmd][pillar]") {
    const auto overnight = make_pillar_quote_key("EUR", "1D", "SPOT", spot_start, first_end);
    const auto six_month = make_pillar_quote_key("EUR", "6M", "SPOT", spot_start, first_end);
    const auto three_month = make_pillar_quote_key("EUR", "3M", "SPOT", spot_start, first_end);

    CHECK(six_month.qualifier == "EUR/0D/6M");
    CHECK(pillar_series_uri(six_month) ==
          "oresmd://ir/EUR?type=series&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=6M");
    CHECK(pillar_series_uri(six_month) != pillar_series_uri(three_month));
    CHECK(pillar_series_uri(six_month) != pillar_series_uri(overnight));
}

TEST_CASE("a_pillar_whose_template_names_a_tenor_is_keyed_by_that_tenor", "[oresmd][pillar]") {
    // 2026-01-28 to 2026-03-19 is the 50 days the same pillar keys without a tenor,
    // so the date the run resolved does not reach the key.
    const auto key = make_pillar_quote_key("GBP", "1D", "SPOT", spot_start, first_end, "1M");

    CHECK(key.qualifier == "GBP/0D/1D");
    CHECK(key.point == "1M");
    CHECK(pillar_datum_uri(key) ==
          "oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1M");
}

TEST_CASE("a_pillar_that_starts_on_a_date_ends_on_its_end_date", "[oresmd][pillar]") {
    // ORE refuses a swap that starts on a date and ends on a period, so a named tenor
    // cannot reach a dated pillar's point even when its template has one.
    const auto key = make_pillar_quote_key("USD", "1D", "1F", spot_start, first_end, "3M");

    CHECK(key.qualifier == "USD/20260128/1D");
    CHECK(key.point == "20260319");
    CHECK(pillar_datum_uri(key) ==
          "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=20260128"
          "&tenor=1D&term=20260319");
}

TEST_CASE("a_named_tenor_and_the_days_to_the_same_end_are_two_quotes_one_series",
          "[oresmd][pillar]") {
    const auto named = make_pillar_quote_key("GBP", "1D", "SPOT", spot_start, first_end, "1M");
    const auto counted = make_pillar_quote_key("GBP", "1D", "SPOT", spot_start, first_end);

    CHECK(named.point != counted.point);
    CHECK(pillar_datum_uri(named) != pillar_datum_uri(counted));
    CHECK(pillar_series_uri(named) == pillar_series_uri(counted));
}

TEST_CASE("a_pillar_that_starts_at_a_period_is_keyed_by_both_of_its_tenors", "[oresmd][pillar]") {
    // A template that names both ends of a 3M to 6M pillar names a 3M start and a 6M end,
    // so the quote is the one a curve configuration writes down and it does not move with
    // the day the run resolves the start to.
    const auto key = make_pillar_quote_key("USD", "1D", "3M", spot_start, first_end, "6M");

    CHECK(key.qualifier == "USD/3M/1D");
    CHECK(key.point == "6M");
    CHECK(pillar_series_uri(key) ==
          "oresmd://ir/USD?type=series&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D");
    CHECK(pillar_datum_uri(key) ==
          "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=3M&tenor=1D"
          "&term=6M");
}

TEST_CASE("the_same_period_pillar_on_two_days_is_one_series", "[oresmd][pillar]") {
    constexpr auto later_start = std::chrono::year{2026} / 1 / 29;
    constexpr auto later_end = std::chrono::year{2026} / 3 / 20;
    const auto today = make_pillar_quote_key("USD", "1D", "3M", spot_start, first_end, "6M");
    const auto later = make_pillar_quote_key("USD", "1D", "3M", later_start, later_end, "6M");

    CHECK(pillar_series_uri(today) == pillar_series_uri(later));
}

TEST_CASE("a_period_start_whose_template_names_no_end_is_keyed_by_its_dates", "[oresmd][pillar]") {
    // Without a named end there is no second period, and ORE refuses one period and one
    // date, so the start is written as the date the run resolved it to.
    const auto key = make_pillar_quote_key("USD", "1D", "3M", spot_start, first_end);

    CHECK(key.qualifier == "USD/20260128/1D");
    CHECK(key.point == "20260319");
}
