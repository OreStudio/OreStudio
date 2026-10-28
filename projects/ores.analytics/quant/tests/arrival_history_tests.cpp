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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.analytics.quant/service/arrival_history.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>

using ores::analytics::quant::service::arrival_history;

TEST_CASE("an arrival history counts ticks by slot and by group", "[arrival_history]") {
    arrival_history history(1.0, 60);
    history.tick(0.2, 0);
    history.tick(0.9, 1);
    history.tick(0.95, 1);
    history.tick(3.5, 0);
    history.sample(0.5, 2.0, true);
    history.sample(0.6, 1.0, false);

    const auto bins = history.since(0.0);
    REQUIRE(bins.size() == 2);
    CHECK(bins[0].slot == 0);
    CHECK(bins[0].counts == std::vector<std::uint32_t>{1, 2});
    CHECK(bins[0].pace_sum == 3.0);
    CHECK(bins[0].active_sum == 1.0);
    CHECK(bins[0].samples == 2);
    CHECK(bins[1].slot == 3);
    CHECK(bins[1].counts == std::vector<std::uint32_t>{1});
}

TEST_CASE("an arrival history keeps a span of slots, so a quiet stretch ages out",
          "[arrival_history]") {
    arrival_history history(0.1, 100);
    history.tick(0.05, 0);
    history.tick(5.0, 0);
    CHECK(history.since(0.0).size() == 2);
    history.tick(10.05, 0);
    const auto bins = history.since(0.0);
    REQUIRE(bins.size() == 2);
    CHECK(bins.front().slot == 50);
}

TEST_CASE("an arrival history places a late reading in its own slot", "[arrival_history]") {
    arrival_history history(1.0, 60);
    history.tick(5.0, 0);
    history.sample(3.2, 1.0, false);
    const auto bins = history.since(0.0);
    REQUIRE(bins.size() == 2);
    CHECK(bins[0].slot == 3);
    CHECK(bins[1].slot == 5);
}

TEST_CASE("an arrival history drops a tick older than its span", "[arrival_history]") {
    arrival_history history(1.0, 10);
    history.tick(20.0, 0);
    history.tick(5.0, 0);
    history.sample(9.0, 1.0, false);
    const auto bins = history.since(0.0);
    REQUIRE(bins.size() == 1);
    CHECK(bins.front().slot == 20);
}

TEST_CASE("an arrival history refuses a slot or capacity that is not positive",
          "[arrival_history]") {
    CHECK_THROWS_AS(arrival_history(0.0, 10), std::invalid_argument);
    CHECK_THROWS_AS(arrival_history(1.0, 0), std::invalid_argument);
}
