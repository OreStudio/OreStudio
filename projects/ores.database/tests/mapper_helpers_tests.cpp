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
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>

using namespace ores::database::repository;

TEST_CASE("a_row_that_is_current_has_no_validity_end", "[repository][mapper_helpers]") {
    const auto current = db_timestamp::max();

    CHECK_FALSE(validity_end(current).has_value());
}

TEST_CASE("a_row_that_ended_has_the_instant_it_ended", "[repository][mapper_helpers]") {
    const auto ended = timestamp_to_timepoint("2026-10-11 16:52:50+00");

    const auto end = validity_end(ended);
    REQUIRE(end.has_value());
    CHECK(ores::platform::time::datetime::to_iso8601_utc(*end) == "2026-10-11 16:52:50.000000Z");
}

TEST_CASE("the instant before never is an instant and not the sentinel",
          "[repository][mapper_helpers]") {
    // The sentinel is one value, so the boundary is that value and not the year
    // it falls in: an instant a microsecond earlier still ends the row.
    const auto almost = db_timestamp::max() - std::chrono::microseconds(1);

    const auto end = validity_end(almost);
    REQUIRE(end.has_value());
    CHECK(*end == almost);
}
