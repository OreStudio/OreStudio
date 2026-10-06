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
#include "ores.service/service/rate_limiter.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

using ores::service::service::rate_limiter;
using std::chrono::seconds;
using time_point = std::chrono::system_clock::time_point;

namespace {

const std::string tags("[rate_limiter]");

}

TEST_CASE("a key may burst up to its bucket", tags) {
    time_point now{};
    rate_limiter limiter(60, seconds(60), 3, [&] { return now; });

    CHECK(limiter.allow("svc").allowed);
    CHECK(limiter.allow("svc").allowed);
    CHECK(limiter.allow("svc").allowed);

    const auto refused = limiter.allow("svc");
    CHECK_FALSE(refused.allowed);
    CHECK(refused.retry_after > std::chrono::milliseconds::zero());
}

TEST_CASE("a refusal says when the next token comes", tags) {
    time_point now{};
    // One token a second, a bucket of one.
    rate_limiter limiter(1, seconds(1), 1, [&] { return now; });

    CHECK(limiter.allow("svc").allowed);
    const auto refused = limiter.allow("svc");
    REQUIRE_FALSE(refused.allowed);
    // The bucket is empty and refills at one a second, so the wait is a second.
    CHECK(refused.retry_after == seconds(1));

    now += std::chrono::milliseconds(1000);
    CHECK(limiter.allow("svc").allowed);
}

TEST_CASE("keys have their own buckets", tags) {
    time_point now{};
    rate_limiter limiter(1, seconds(1), 1, [&] { return now; });

    CHECK(limiter.allow("a").allowed);
    CHECK(limiter.allow("b").allowed);
    CHECK_FALSE(limiter.allow("a").allowed);
}

TEST_CASE("a quiet key refills to its burst and no further", tags) {
    time_point now{};
    rate_limiter limiter(1, seconds(1), 2, [&] { return now; });

    CHECK(limiter.allow("svc").allowed);
    CHECK(limiter.allow("svc").allowed);
    CHECK_FALSE(limiter.allow("svc").allowed);

    // A long quiet spell refills the bucket to its burst of two, not to a
    // minute's worth.
    now += seconds(60);
    CHECK(limiter.allow("svc").allowed);
    CHECK(limiter.allow("svc").allowed);
    CHECK_FALSE(limiter.allow("svc").allowed);
}

TEST_CASE("forget drops a key's bucket", tags) {
    time_point now{};
    rate_limiter limiter(1, seconds(1), 1, [&] { return now; });

    CHECK(limiter.allow("svc").allowed);
    CHECK_FALSE(limiter.allow("svc").allowed);
    CHECK(limiter.tracked() == 1);

    limiter.forget("svc");
    CHECK(limiter.tracked() == 0);
    CHECK(limiter.allow("svc").allowed);
}

TEST_CASE("a limit of nothing admits every request", tags) {
    time_point now{};
    rate_limiter limiter(0, seconds(1), 0, [&] { return now; });

    for (int i = 0; i < 100; ++i)
        CHECK(limiter.allow("svc").allowed);
}
