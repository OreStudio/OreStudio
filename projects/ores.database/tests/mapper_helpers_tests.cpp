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
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.database.tests");
const std::string tags("[repository][mapper_helpers]");

}

using namespace ores::database::repository;

TEST_CASE("a_row_that_is_current_has_no_validity_end", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    const auto current = make_timestamp(MAX_TIMESTAMP_NAIVE, lg);

    CHECK_FALSE(validity_end(current.value()).has_value());
}

TEST_CASE("a_row_that_ended_has_the_instant_it_ended", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    const auto ended = make_timestamp("2026-10-11 16:52:50", lg);

    const auto end = validity_end(ended.value());
    REQUIRE(end.has_value());
    CHECK(*end == timestamp_to_timepoint(ended.value()));
    CHECK(ores::platform::time::datetime::to_iso8601_utc(*end) == "2026-10-11 16:52:50Z");
}
