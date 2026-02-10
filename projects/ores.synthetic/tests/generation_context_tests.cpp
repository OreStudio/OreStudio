/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.synthetic/domain/generation_context.hpp"

#include <set>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace {

const std::string tags("[generation_context]");

using namespace ores::synthetic::domain;

}

TEST_CASE("seeded_context_returns_same_seed", tags) {
    const std::uint64_t seed = 42;
    generation_context ctx(seed);
    REQUIRE(ctx.seed() == seed);
}

TEST_CASE("default_context_has_nonzero_seed", tags) {
    generation_context ctx;
    REQUIRE(ctx.seed() != 0);
}

TEST_CASE("same_seed_produces_same_random_sequence", tags) {
    const std::uint64_t seed = 12345;
    generation_context ctx1(seed);
    generation_context ctx2(seed);

    for (int i = 0; i < 20; ++i) {
        REQUIRE(ctx1.random_int(0, 1000) == ctx2.random_int(0, 1000));
    }
}

TEST_CASE("random_int_respects_bounds", tags) {
    generation_context ctx(99);
    for (int i = 0; i < 100; ++i) {
        int val = ctx.random_int(10, 20);
        REQUIRE(val >= 10);
        REQUIRE(val <= 20);
    }
}

TEST_CASE("random_int_single_value_range", tags) {
    generation_context ctx(42);
    for (int i = 0; i < 10; ++i) {
        REQUIRE(ctx.random_int(5, 5) == 5);
    }
}

TEST_CASE("random_bool_returns_both_values", tags) {
    generation_context ctx(42);
    bool seen_true = false;
    bool seen_false = false;
    for (int i = 0; i < 100 && !(seen_true && seen_false); ++i) {
        if (ctx.random_bool(0.5)) {
            seen_true = true;
        } else {
            seen_false = true;
        }
    }
    REQUIRE(seen_true);
    REQUIRE(seen_false);
}

TEST_CASE("generate_uuid_produces_v7_format", tags) {
    generation_context ctx(42);
    auto uuid = ctx.generate_uuid();

    REQUIRE(!uuid.is_nil());

    // Check version nibble (upper nibble of byte 6) is 0x7
    REQUIRE((uuid.data[6] >> 4) == 0x07);

    // Check variant bits (upper 2 bits of byte 8) are 0b10
    REQUIRE((uuid.data[8] >> 6) == 0x02);
}

TEST_CASE("generate_uuid_produces_unique_values", tags) {
    generation_context ctx(42);
    std::set<std::string> seen;
    for (int i = 0; i < 50; ++i) {
        auto uuid = ctx.generate_uuid();
        auto str = boost::uuids::to_string(uuid);
        REQUIRE(seen.find(str) == seen.end());
        seen.insert(str);
    }
}

TEST_CASE("past_timepoint_returns_time_before_now", tags) {
    generation_context ctx(42);
    auto now = std::chrono::system_clock::now();
    for (int i = 0; i < 10; ++i) {
        auto tp = ctx.past_timepoint(3);
        REQUIRE(tp < now);
    }
}

TEST_CASE("alphanumeric_produces_correct_length", tags) {
    generation_context ctx(42);
    REQUIRE(ctx.alphanumeric(0).empty());
    REQUIRE(ctx.alphanumeric(1).size() == 1);
    REQUIRE(ctx.alphanumeric(10).size() == 10);
    REQUIRE(ctx.alphanumeric(64).size() == 64);
}

TEST_CASE("alphanumeric_contains_only_valid_characters", tags) {
    generation_context ctx(42);
    const std::string valid =
        "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    auto result = ctx.alphanumeric(200);
    for (char c : result) {
        REQUIRE(valid.find(c) != std::string::npos);
    }
}

TEST_CASE("pick_from_vector_returns_element", tags) {
    generation_context ctx(42);
    std::vector<int> items = {10, 20, 30, 40, 50};
    std::set<int> valid_set(items.begin(), items.end());

    for (int i = 0; i < 20; ++i) {
        int picked = ctx.pick(items);
        REQUIRE(valid_set.count(picked) == 1);
    }
}

TEST_CASE("pick_from_empty_vector_throws", tags) {
    generation_context ctx(42);
    std::vector<int> empty;
    REQUIRE_THROWS_AS(ctx.pick(empty), std::out_of_range);
}

TEST_CASE("pick_from_array_returns_element", tags) {
    generation_context ctx(42);
    std::array<std::string, 3> items = {"alpha", "beta", "gamma"};
    std::set<std::string> valid_set(items.begin(), items.end());

    for (int i = 0; i < 20; ++i) {
        const auto& picked = ctx.pick(items);
        REQUIRE(valid_set.count(picked) == 1);
    }
}
