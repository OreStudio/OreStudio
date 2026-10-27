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
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>

namespace {

using ores::compute::messaging::claimable;
using ores::compute::messaging::claimed_by_another;
using ores::compute::messaging::decide_submit;
using ores::compute::messaging::submit_action;

}

TEST_CASE("decide_submit_records_the_outcome_of_a_result_that_waits_or_runs", "[messaging]") {
    CHECK(decide_submit(2) == submit_action::accept);
    CHECK(decide_submit(4) == submit_action::accept);
}

TEST_CASE("decide_submit_does_not_revive_a_retired_result", "[messaging]") {
    CHECK(decide_submit(1) == submit_action::ignore_inactive);
}

TEST_CASE("decide_submit_ignores_a_second_outcome_for_a_done_result", "[messaging]") {
    CHECK(decide_submit(5) == submit_action::ignore_final);
}

TEST_CASE("decide_submit_records_the_outcome_for_a_state_it_does_not_know", "[messaging]") {
    // Only a retired or a done result is protected. Any other state is recorded as before.
    CHECK(decide_submit(0) == submit_action::accept);
    CHECK(decide_submit(3) == submit_action::accept);
    CHECK(decide_submit(99) == submit_action::accept);
}

TEST_CASE("claimed_by_another_is_false_for_a_result_with_no_holder", "[messaging]") {
    // A result handed out by the push queue has no entry in the ledger.
    const auto host = boost::uuids::random_generator()();

    CHECK(!claimed_by_another(std::nullopt, host));
    CHECK(!claimed_by_another(std::nullopt, std::nullopt));
}

TEST_CASE("claimed_by_another_is_false_for_the_holder_and_true_for_any_other_host", "[messaging]") {
    const auto holder = boost::uuids::random_generator()();
    const auto other = boost::uuids::random_generator()();

    CHECK(!claimed_by_another(holder, holder));
    CHECK(claimed_by_another(holder, other));
}

TEST_CASE("claimed_by_another_is_true_when_the_submitter_names_no_host", "[messaging]") {
    const auto holder = boost::uuids::random_generator()();

    CHECK(claimed_by_another(holder, std::nullopt));
}

TEST_CASE("claimable_runs_an_unsent_result_on_any_attempt", "[messaging]") {
    CHECK(claimable(2, 1));
    CHECK(claimable(2, 2));
}

TEST_CASE("claimable_runs_a_result_in_progress_only_on_a_later_attempt", "[messaging]") {
    CHECK(!claimable(4, 1));
    CHECK(claimable(4, 2));
    CHECK(claimable(4, 3));
}

TEST_CASE("claimable_never_runs_a_retired_or_done_result", "[messaging]") {
    CHECK(!claimable(1, 1));
    CHECK(!claimable(1, 2));
    CHECK(!claimable(5, 1));
    CHECK(!claimable(5, 2));
}
