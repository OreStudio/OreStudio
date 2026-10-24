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
#include "ores.marketdata.api/domain/market_clock.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[market_clock]");

using namespace std::chrono_literals;
using ores::marketdata::domain::fixed_market_clock;
using ores::marketdata::domain::start_date;
using ores::marketdata::domain::system_market_clock;

}

TEST_CASE("a fixed clock gives the same date at every call", tags) {
    const fixed_market_clock clock(std::chrono::sys_days{2026y / 10 / 23});

    CHECK(clock.today() == std::chrono::sys_days{2026y / 10 / 23});
    CHECK(clock.today() == clock.today());
    CHECK(clock.today_text() == "2026-10-23");
}

TEST_CASE("the system clock gives the date of the host in UTC", tags) {
    const system_market_clock clock;
    const auto before = std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now());

    const auto today = clock.today();

    const auto after = std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now());
    CHECK(today >= before);
    CHECK(today <= after);
    CHECK(clock.today_text().size() == 10);
}

TEST_CASE("a config that names a date keeps it and one that names none takes the clock's", tags) {
    const fixed_market_clock clock(std::chrono::sys_days{2026y / 10 / 23});

    CHECK(start_date("2026-05-05", clock) == "2026-05-05");
    CHECK(start_date("", clock) == "2026-10-23");
}

TEST_CASE("two feeds that share a clock start from the same date", tags) {
    const fixed_market_clock clock(std::chrono::sys_days{2026y / 1 / 2});

    CHECK(start_date("", clock) == start_date("", clock));
}
