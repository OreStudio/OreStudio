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
 * The end date is the observation's point, so it leaves the identity. That is what
 * lets the reader ask for a series by its key without already knowing which of the
 * series' points it wants.
 */

namespace {

using ores::marketdata::core::make_pillar_quote_key;
using ores::marketdata::core::pillar_series_uri;

constexpr auto spot_start = std::chrono::year{2026} / 1 / 28;
constexpr auto first_end = std::chrono::year{2026} / 3 / 19;
constexpr auto second_end = std::chrono::year{2026} / 4 / 29;

}

TEST_CASE("a_pillar_that_starts_at_spot_keys_0d_in_its_start_slot", "[oresmd][pillar]") {
    const auto key = make_pillar_quote_key("USD", "SPOT", spot_start, first_end);

    CHECK(key.series_type == "IR_SWAP");
    CHECK(key.metric == "RATE");
    CHECK(key.qualifier == "USD/0D/1D");
    CHECK(key.point == "20260319");
    CHECK(pillar_series_uri(key) ==
          "oresmd://ir/usd?tenor=1d&settle=0D&type=quote&metric=rate&quote=ir_swap");
}

TEST_CASE("a_pillar_that_starts_on_a_meeting_keys_the_date_it_starts_on", "[oresmd][pillar]") {
    const auto key = make_pillar_quote_key("USD", "1F", spot_start, first_end);

    CHECK(key.qualifier == "USD/20260128/1D");
    CHECK(key.point == "20260319");
    CHECK(pillar_series_uri(key) ==
          "oresmd://ir/usd?tenor=1d&settle=20260128&type=quote&metric=rate&quote=ir_swap");
}

TEST_CASE("a_pillars_end_date_is_its_point_and_not_part_of_its_series", "[oresmd][pillar]") {
    const auto short_pillar = make_pillar_quote_key("USD", "1F", spot_start, first_end);
    const auto long_pillar = make_pillar_quote_key("USD", "1F", spot_start, second_end);

    CHECK(short_pillar.point != long_pillar.point);
    CHECK(pillar_series_uri(short_pillar) == pillar_series_uri(long_pillar));
}

TEST_CASE("two_pillars_are_two_series", "[oresmd][pillar]") {
    const auto spot = make_pillar_quote_key("USD", "SPOT", spot_start, first_end);
    const auto dated =
        make_pillar_quote_key("USD", "1F", std::chrono::year{2026} / 1 / 28, second_end);

    CHECK(pillar_series_uri(spot) != pillar_series_uri(dated));
    CHECK(pillar_series_uri(spot) ==
          "oresmd://ir/usd?tenor=1d&settle=0D&type=quote&metric=rate&quote=ir_swap");
    CHECK(pillar_series_uri(dated) ==
          "oresmd://ir/usd?tenor=1d&settle=20260128&type=quote&metric=rate&quote=ir_swap");
}
