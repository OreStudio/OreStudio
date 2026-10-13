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
#include "ores.compute.api/messaging/assignment_expiry.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

const std::string tags("[messaging]");

using ores::compute::messaging::assignment_expired;
using ores::compute::messaging::token_expiry;
using std::chrono::seconds;
using std::chrono::system_clock;

/// A token with the given base64url claims, and no signature worth checking.
std::string token_with(const std::string& claims) {
    return "e30." + claims + ".c2ln";
}

system_clock::time_point at(long long epoch_seconds) {
    return system_clock::time_point{seconds{epoch_seconds}};
}

}

TEST_CASE("token_expiry_reads_the_exp_claim", tags) {
    const auto expiry = token_expiry(token_with("eyJleHAiOjEwMDB9"));

    REQUIRE(expiry.has_value());
    CHECK(*expiry == at(1000));
}

TEST_CASE("token_expiry_reads_exp_among_other_claims", tags) {
    const auto expiry =
        token_expiry(token_with("eyJzdWIiOiJ4IiwiZXhwIjoxMjM0NTY3ODkwLCJpYXQiOjF9"));

    REQUIRE(expiry.has_value());
    CHECK(*expiry == at(1234567890));
}

TEST_CASE("token_expiry_allows_a_space_after_the_colon", tags) {
    const auto expiry = token_expiry(token_with("eyJleHAiOiAyMDAwfQ"));

    REQUIRE(expiry.has_value());
    CHECK(*expiry == at(2000));
}

TEST_CASE("token_expiry_is_unknown_without_an_exp_claim", tags) {
    CHECK(!token_expiry(token_with("eyJzdWIiOiJ4In0")).has_value());
}

TEST_CASE("token_expiry_is_unknown_for_a_claim_that_is_not_a_number", tags) {
    CHECK(!token_expiry(token_with("eyJleHAiOiJzb29uIn0")).has_value());
}

TEST_CASE("token_expiry_is_unknown_for_text_that_is_not_a_token", tags) {
    CHECK(!token_expiry("").has_value());
    CHECK(!token_expiry("not-a-token").has_value());
    CHECK(!token_expiry("a.b").has_value());
    CHECK(!token_expiry(token_with("!!!not base64!!!")).has_value());
}

TEST_CASE("token_expiry_is_unknown_for_an_exponent_form", tags) {
    // 1.7e9 must not read as 1, which would make a live token look ended.
    CHECK(!token_expiry(token_with("eyJleHAiOjEuN2U5fQ")).has_value());
}

TEST_CASE("token_expiry_is_unknown_for_a_run_of_digits_too_long_to_be_a_date", tags) {
    CHECK(!token_expiry(
               token_with("eyJleHAiOjk5OTk5OTk5OTk5OTk5OTk5OTk5OTk5OTk5OTk5OTk5OTk5OTk5OTl9"))
               .has_value());
}

TEST_CASE("token_expiry_reads_a_fractional_date_as_its_whole_seconds", tags) {
    const auto expiry = token_expiry(token_with("eyJleHAiOjE3NjAwMDAwMDAuNX0"));

    REQUIRE(expiry.has_value());
    CHECK(*expiry == at(1760000000));
}

TEST_CASE("assignment_expired_when_the_token_has_ended", tags) {
    const auto token = token_with("eyJleHAiOjEwMDB9");

    CHECK(assignment_expired(token, at(1001)));
    CHECK(assignment_expired(token, at(1000)));
    CHECK(!assignment_expired(token, at(999)));
}

TEST_CASE("assignment_is_not_expired_when_the_expiry_is_unknown", tags) {
    // A node that cannot read the expiry must start the job and let the storage
    // service decide, because refusing work on a guess would stall the queue.
    CHECK(!assignment_expired("", at(1'000'000'000)));
    CHECK(!assignment_expired("not-a-token", at(1'000'000'000)));
}
