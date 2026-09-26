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
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/faker/datetime.hpp"
#include <catch2/catch_test_macros.hpp>
#include <regex>
#include <string>

namespace {

const std::string_view test_suite("ores.utility.tests");
const std::string tags("[faker]");

}

using ores::utility::faker::datetime;
using namespace ores::logging;

TEST_CASE("make_timepoint_places_the_parts_in_utc", tags) {
    auto lg(make_logger(test_suite));

    const auto tp = datetime::make_timepoint(2024, 3, 15, 13, 45, 30);

    BOOST_LOG_SEV(lg, info) << "Time point: " << ores::platform::time::datetime::to_iso8601_utc(tp);

    CHECK(ores::platform::time::datetime::to_iso8601_utc(tp) == "2024-03-15 13:45:30Z");
}

TEST_CASE("make_timepoint_defaults_the_time_of_day_to_midnight", tags) {
    auto lg(make_logger(test_suite));

    const auto tp = datetime::make_timepoint(2024, 3, 15);

    CHECK(ores::platform::time::datetime::to_iso8601_utc(tp) == "2024-03-15 00:00:00Z");
}

TEST_CASE("past_timepoint_stays_inside_the_declared_window", tags) {
    auto lg(make_logger(test_suite));

    const auto lower = datetime::make_timepoint(1970, 1, 1);
    const auto upper = datetime::make_timepoint(2039, 1, 1);

    for (int i = 0; i < 20; ++i) {
        const auto tp = datetime::past_timepoint();
        CHECK(tp >= lower);
        CHECK(tp < upper);
    }
}

TEST_CASE("past_string_is_a_database_timestamp_between_1970_and_2038", tags) {
    auto lg(make_logger(test_suite));

    const std::regex shape(R"(\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2})");

    for (int i = 0; i < 20; ++i) {
        const auto s = datetime::past_string();

        BOOST_LOG_SEV(lg, info) << "Timestamp: " << s;

        CHECK(std::regex_match(s, shape));
        CHECK(s.substr(0, 4) >= "1970");
        CHECK(s.substr(0, 4) <= "2038");
    }
}
