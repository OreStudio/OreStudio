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
#include "ores.compute.core/messaging/submit_decision.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[messaging]");

using ores::compute::messaging::decide_submit;
using ores::compute::messaging::submit_action;

}

TEST_CASE("decide_submit_records_the_outcome_of_a_result_that_waits_or_runs", tags) {
    CHECK(decide_submit(2) == submit_action::accept);
    CHECK(decide_submit(4) == submit_action::accept);
}

TEST_CASE("decide_submit_does_not_revive_a_retired_result", tags) {
    CHECK(decide_submit(1) == submit_action::ignore_inactive);
}

TEST_CASE("decide_submit_ignores_a_second_outcome_for_a_done_result", tags) {
    CHECK(decide_submit(5) == submit_action::ignore_final);
}

TEST_CASE("decide_submit_records_the_outcome_for_a_state_it_does_not_know", tags) {
    // Only a retired or a done result is protected. Any other state is recorded as before.
    CHECK(decide_submit(0) == submit_action::accept);
    CHECK(decide_submit(3) == submit_action::accept);
    CHECK(decide_submit(99) == submit_action::accept);
}
