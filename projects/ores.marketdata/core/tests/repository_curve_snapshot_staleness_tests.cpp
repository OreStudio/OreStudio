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
#include "ores.marketdata.core/repository/curve_snapshot_staleness.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace {

const std::string tags("[staleness]");

using ores::marketdata::repository::observation_record;
using ores::marketdata::repository::staleness_summary;
using ores::marketdata::repository::summarise_staleness;

const auto read_instant = std::chrono::system_clock::time_point{std::chrono::seconds{2000000000}};

// One point whose instant is the given number of seconds before the read. Its
// record time is not what an age is measured from, so it is left at the read's
// own instant.
observation_record point_at(std::int64_t age_seconds) {
    observation_record record;
    record.observation.observation_datetime = read_instant - std::chrono::seconds(age_seconds);
    record.recorded_at = read_instant;
    return record;
}

}

TEST_CASE("an empty snapshot is in no way mixed", tags) {
    const auto summary = summarise_staleness({}, read_instant);

    CHECK(summary.oldest_age_seconds == 0);
    CHECK(summary.spread_seconds == 0);
    CHECK_FALSE(summary.warning);
}

TEST_CASE("points that share one instant have no spread however old they are", tags) {
    // Three days old, and every point the same age: one market state, drawn
    // late. The age is not the warning; the spread is.
    const auto summary =
        summarise_staleness({point_at(259200), point_at(259200), point_at(259200)}, read_instant);

    CHECK(summary.oldest_age_seconds == 259200);
    CHECK(summary.spread_seconds == 0);
    CHECK_FALSE(summary.warning);
}

TEST_CASE("spread_under_the_threshold_is_not_a_warning", tags) {
    const auto summary = summarise_staleness({point_at(60), point_at(1800)}, read_instant);

    CHECK(summary.oldest_age_seconds == 1800);
    CHECK(summary.spread_seconds == 1740);
    CHECK_FALSE(summary.warning);
}

TEST_CASE("spread_at_the_threshold_warns", tags) {
    const auto summary = summarise_staleness({point_at(0), point_at(1800)}, read_instant);

    CHECK(summary.oldest_age_seconds == 1800);
    CHECK(summary.spread_seconds == 1800);
    CHECK(summary.warning);
}

TEST_CASE("a_curve_stitched_across_market_horizons_warns", tags) {
    // A minute-old pillar beside one three days old: the case the whole
    // capability exists for.
    const auto summary =
        summarise_staleness({point_at(60), point_at(120), point_at(259200)}, read_instant);

    CHECK(summary.oldest_age_seconds == 259200);
    CHECK(summary.spread_seconds == 259140);
    CHECK(summary.warning);
}

TEST_CASE("a_point_after_the_read_carries_no_age", tags) {
    // A snapshot read as of a past instant holds only points at or before it,
    // but a point exactly at it is age zero and counts, and one after it
    // carries no age at all rather than a negative one.
    const auto summary =
        summarise_staleness({point_at(0), point_at(-60), point_at(600)}, read_instant);

    CHECK(summary.oldest_age_seconds == 600);
    CHECK(summary.spread_seconds == 600);
    CHECK_FALSE(summary.warning);
}

TEST_CASE("the_newest_point_is_age_zero_and_sets_the_spread", tags) {
    // A point exactly at the read instant counts: without it the spread would
    // read as zero however far back the rest of the curve went.
    const auto summary = summarise_staleness({point_at(0), point_at(3600)}, read_instant);

    CHECK(summary.oldest_age_seconds == 3600);
    CHECK(summary.spread_seconds == 3600);
    CHECK(summary.warning);
}

TEST_CASE("a_snapshot_with_no_aged_point_is_in_no_way_mixed", tags) {
    const auto summary = summarise_staleness({point_at(0), point_at(-30)}, read_instant);

    CHECK(summary.oldest_age_seconds == 0);
    CHECK(summary.spread_seconds == 0);
    CHECK_FALSE(summary.warning);
}
