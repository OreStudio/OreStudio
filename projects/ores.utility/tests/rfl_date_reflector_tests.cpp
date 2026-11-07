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
#include "ores.utility/rfl/reflectors.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <stdexcept>
#include <string>

namespace {

struct holds_date {
    std::chrono::year_month_day when;
};

}

using namespace ores::logging;
using std::chrono::day;
using std::chrono::month;
using std::chrono::year;
using std::chrono::year_month_day;

TEST_CASE("date_travels_as_its_iso_text", "[rfl][date]") {
    auto lg(make_logger(test_suite));

    const auto value = year{2026} / month{10} / day{31};
    CHECK(rfl::Reflector<year_month_day>::from(value) == "2026-10-31");
    CHECK(rfl::Reflector<year_month_day>::to("2026-10-31") == value);
}

TEST_CASE("an_absent_date_travels_as_the_empty_string", "[rfl][date]") {
    auto lg(make_logger(test_suite));

    /*
     * The domain states an absent date as the default value, so the default has
     * to survive the wire: it is the only spelling of absent a reader has.
     */
    CHECK(rfl::Reflector<year_month_day>::from(year_month_day{}) == "");
    CHECK(rfl::Reflector<year_month_day>::to("") == year_month_day{});

    holds_date sut{.when = year_month_day{}};
    const auto written = rfl::json::write(sut);
    const auto read = rfl::json::read<holds_date>(written);
    REQUIRE(read);
    CHECK(read->when == year_month_day{});
}

TEST_CASE("a_date_that_is_not_a_date_refuses_to_be_written", "[rfl][date]") {
    auto lg(make_logger(test_suite));

    /*
     * The 31st of September is not a day. Writing it as the empty string would
     * state it as absent, and a reader would store the absent date in its
     * place; two such ends of a window would arrive equal. So it refuses, and
     * the caller that invented it is the one told.
     */
    const auto invented = year{2026} / month{9} / day{31};
    REQUIRE_FALSE(invented.ok());
    CHECK_THROWS_AS(rfl::Reflector<year_month_day>::from(invented), std::runtime_error);

    const holds_date sut{.when = invented};
    CHECK_THROWS(rfl::json::write(sut));
    CHECK_THROWS(rfl::msgpack::write(sut));
}
