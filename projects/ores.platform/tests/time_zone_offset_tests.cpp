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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.platform/time/zone_offset.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>

namespace {

using namespace std::chrono;
using ores::platform::time::zone_offset_at;

system_clock::time_point utc_at(int year, unsigned month, unsigned day, int hour) {
    return system_clock::time_point{sys_days{std::chrono::year{year} /
                                             std::chrono::month{month} /
                                             std::chrono::day{day}} +
                                    hours{hour}};
}

}

TEST_CASE("an_offset_follows_the_zone_across_a_daylight_saving_change",
          "[time][zone_offset]") {
    /// The expectations are written out rather than read from a library, so this
    /// holds on a platform whose library carries no time zone database at all.
    CHECK(*zone_offset_at("America/New_York", utc_at(2026, 7, 1, 12)) == hours{-4});
    CHECK(*zone_offset_at("America/New_York", utc_at(2026, 12, 1, 12)) == hours{-5});
    CHECK(*zone_offset_at("Europe/London", utc_at(2026, 1, 15, 12)) == hours{0});
    CHECK(*zone_offset_at("Europe/London", utc_at(2026, 7, 15, 12)) == hours{1});
}

TEST_CASE("an_offset_is_the_zone_against_utc", "[time][zone_offset]") {
    CHECK(*zone_offset_at("UTC", utc_at(2026, 7, 1, 12)) == hours{0});
    CHECK(*zone_offset_at("Asia/Tokyo", utc_at(2026, 7, 1, 12)) == hours{9});
    /// A zone an hour and a half east of UTC, which whole hours cannot state.
    CHECK(*zone_offset_at("Asia/Kolkata", utc_at(2026, 7, 1, 12)) == minutes{330});
    CHECK(*zone_offset_at("Pacific/Auckland", utc_at(2026, 1, 15, 12)) == hours{13});
}

TEST_CASE("an_offset_agrees_with_the_librarys_own_database", "[time][zone_offset]") {
#if defined(__cpp_lib_chrono) && __cpp_lib_chrono >= 201907L
    /// A second reading of the same files, by the library, over three years and
    /// a spread of zones, so a change of season is crossed in both hemispheres.
    for (const char* zone : {"UTC", "America/New_York", "Europe/London", "Europe/Dublin",
                             "Asia/Tokyo", "Asia/Kolkata", "Australia/Sydney",
                             "Pacific/Auckland", "America/Sao_Paulo", "Africa/Cairo"}) {
        const auto* expected_zone = locate_zone(zone);
        for (auto at = utc_at(2025, 1, 1, 0); at < utc_at(2028, 1, 1, 0); at += hours{6}) {
            const auto expected = expected_zone->get_info(at).offset;
            const auto read = zone_offset_at(zone, at);
            REQUIRE(read.has_value());
            if (*read != expected)
                FAIL("zone " << zone << " at " << at.time_since_epoch().count()
                             << " read " << read->count() << " where the library says "
                             << expected.count());
        }
    }
#else
    SUCCEED("the library carries no time zone database to compare against");
#endif
}

TEST_CASE("an_offset_is_nothing_for_a_zone_the_database_does_not_hold",
          "[time][zone_offset]") {
    CHECK_FALSE(zone_offset_at("Nowhere/Atlantis", utc_at(2026, 7, 1, 12)));
    CHECK_FALSE(zone_offset_at("", utc_at(2026, 7, 1, 12)));
}

TEST_CASE("an_offset_refuses_a_name_that_climbs_out_of_the_database",
          "[time][zone_offset]") {
    CHECK_FALSE(zone_offset_at("../etc/passwd", utc_at(2026, 7, 1, 12)));
    CHECK_FALSE(zone_offset_at("/etc/passwd", utc_at(2026, 7, 1, 12)));
}
