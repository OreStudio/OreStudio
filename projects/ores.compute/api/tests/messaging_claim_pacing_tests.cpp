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
#include "ores.compute.api/messaging/claim_pacing.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

const std::string tags("[messaging]");

using ores::compute::messaging::error_delay;
using ores::compute::messaging::poll_delay;
using std::chrono::milliseconds;

}

TEST_CASE("poll_delay_spreads_the_base_by_a_quarter_either_side", tags) {
    const milliseconds base(8000);

    CHECK(poll_delay(base, 0.0) == milliseconds(6000));
    CHECK(poll_delay(base, 0.5) == milliseconds(8000));
    CHECK(poll_delay(base, 0.999) <= milliseconds(10000));
    CHECK(poll_delay(base, 0.999) > milliseconds(9900));
}

TEST_CASE("poll_delay_clamps_a_random_number_outside_the_range", tags) {
    const milliseconds base(8000);

    CHECK(poll_delay(base, -3.0) == milliseconds(6000));
    CHECK(poll_delay(base, 7.0) == milliseconds(10000));
}

TEST_CASE("error_delay_doubles_with_each_failure_from_one_second", tags) {
    CHECK(error_delay(1, 0.5) == milliseconds(1000));
    CHECK(error_delay(2, 0.5) == milliseconds(2000));
    CHECK(error_delay(3, 0.5) == milliseconds(4000));
    CHECK(error_delay(4, 0.5) == milliseconds(8000));
    CHECK(error_delay(5, 0.5) == milliseconds(16000));
}

TEST_CASE("error_delay_never_passes_thirty_seconds", tags) {
    CHECK(error_delay(6, 0.999) <= milliseconds(30000));
    CHECK(error_delay(50, 0.999) <= milliseconds(30000));
    CHECK(error_delay(50, 0.5) == milliseconds(30000));
}

TEST_CASE("error_delay_treats_no_failures_as_the_first", tags) {
    CHECK(error_delay(0, 0.5) == milliseconds(1000));
    CHECK(error_delay(-4, 0.5) == milliseconds(1000));
}
