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
#include "../src/restore_retry.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace {

const std::string tags("[restore_retry]");

using ores::synthetic::service::restore_retry_policy;
using ores::synthetic::service::retry_restore;
using std::chrono::milliseconds;
using std::chrono::seconds;

// A restore as a test sees it: the feeds each pass leaves, and the waits.
struct scripted_restore {
    std::vector<std::size_t> left_after_each_pass;
    std::vector<milliseconds> waits;
    std::size_t passes = 0;
    bool stop_after_first_wait = false;

    std::size_t pass() {
        const auto i = std::min(passes, left_after_each_pass.size() - 1);
        ++passes;
        return left_after_each_pass[i];
    }
    void sleep(milliseconds span) {
        waits.push_back(span);
    }
    bool stopped() const {
        return stop_after_first_wait && !waits.empty();
    }
    std::size_t run(const restore_retry_policy& policy, std::size_t remaining) {
        return retry_restore(
            policy,
            remaining,
            [this] { return pass(); },
            [this](milliseconds span) { sleep(span); },
            [this] { return stopped(); });
    }
};

milliseconds total(const std::vector<milliseconds>& waits) {
    milliseconds sum{0};
    for (const auto w : waits)
        sum += w;
    return sum;
}

}

TEST_CASE("a_restore_that_left_nothing_makes_no_pass_and_no_wait", tags) {
    scripted_restore s{.left_after_each_pass = {0}};

    const auto left = s.run(restore_retry_policy{}, 0);

    CHECK(left == 0);
    CHECK(s.passes == 0);
    CHECK(s.waits.empty());
}

TEST_CASE("a_feed_that_starts_on_the_second_try_ends_the_restore_after_one_wait", tags) {
    scripted_restore s{.left_after_each_pass = {0}};

    const auto left = s.run(restore_retry_policy{}, 3);

    CHECK(left == 0);
    CHECK(s.passes == 1);
    REQUIRE(s.waits.size() == 1);
    CHECK(s.waits[0] == milliseconds(2000));
}

TEST_CASE("the_waits_double_to_the_longest_wait", tags) {
    scripted_restore s{.left_after_each_pass = {2, 2, 2, 2, 2, 2, 0}};

    const auto left = s.run(restore_retry_policy{}, 3);

    CHECK(left == 0);
    REQUIRE(s.waits.size() == 7);
    CHECK(s.waits[0] == milliseconds(2000));
    CHECK(s.waits[1] == milliseconds(4000));
    CHECK(s.waits[2] == milliseconds(8000));
    CHECK(s.waits[3] == milliseconds(16000));
    CHECK(s.waits[4] == milliseconds(30000));
    CHECK(s.waits[5] == milliseconds(30000));
    CHECK(s.waits[6] == milliseconds(30000));
}

TEST_CASE("a_feed_that_never_starts_ends_the_restore_when_the_budget_is_spent", tags) {
    scripted_restore s{.left_after_each_pass = {1}};
    const restore_retry_policy policy;

    const auto left = s.run(policy, 1);

    CHECK(left == 1);
    CHECK(total(s.waits) <= policy.budget);
    CHECK(total(s.waits) + s.waits.back() > policy.budget);
}

TEST_CASE("a_service_that_is_shutting_down_ends_the_restore_before_the_next_wait", tags) {
    scripted_restore s{.left_after_each_pass = {1}, .stop_after_first_wait = true};

    const auto left = s.run(restore_retry_policy{}, 1);

    CHECK(left == 1);
    CHECK(s.waits.size() == 1);
    CHECK(s.passes == 1);
}

TEST_CASE("a_policy_with_a_short_budget_makes_fewer_tries", tags) {
    scripted_restore s{.left_after_each_pass = {1}};
    const restore_retry_policy policy{
        .first_wait = milliseconds(100), .longest_wait = milliseconds(400), .budget = seconds(1)};

    const auto left = s.run(policy, 1);

    CHECK(left == 1);
    CHECK(total(s.waits) <= policy.budget);
    CHECK(s.waits.front() == milliseconds(100));
    CHECK(s.waits.back() == milliseconds(400));
}
