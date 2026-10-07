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
#include "ores.service/service/cache/run_token_cache.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

using ores::service::service::cache::run_token;
using ores::service::service::cache::run_token_cache;
using ores::service::service::cache::run_token_key;
using std::chrono::seconds;
using time_point = std::chrono::system_clock::time_point;

// The decisions these pin: when a held token is used and when it is exchanged
// again, that a refused exchange holds nothing, that a caller can drop a stale
// token and exchange once, and that the cache is bounded by run. No network and
// no clock are involved, which is why the exchange and the clock are injected.

namespace {

const std::string tags("[run_token_cache]");

run_token_key key_of(const std::string& grant, const std::string& run) {
    return run_token_key{.grant_id = grant, .run_id = run};
}

/// A minter that names each token after its key and the exchange count.
struct counting_minter {
    std::vector<run_token_key>* asked;
    time_point* now;
    seconds lifetime;
    bool* refuse;

    std::optional<run_token> operator()(const run_token_key& key,
                                        std::string_view /*tenant_id*/) const {
        asked->push_back(key);
        if (*refuse)
            return std::nullopt;
        return run_token{.token =
                             key.grant_id + "/" + key.run_id + "#" + std::to_string(asked->size()),
                         .expires_at = *now + lifetime};
    }
};

}

TEST_CASE("a miss mints a token and a fresh token is reused", tags) {
    std::vector<run_token_key> asked;
    time_point now{};
    bool refuse = false;
    run_token_cache cache(
        counting_minter{&asked, &now, seconds(300), &refuse}, 16, seconds(60), [&] { return now; });

    const auto first = cache.token_for(key_of("g1", "r1"), "t1");
    const auto second = cache.token_for(key_of("g1", "r1"), "t1");

    CHECK(first == "g1/r1#1");
    CHECK(second == first);
    CHECK(asked.size() == 1);
}

TEST_CASE("a token close to expiry is exchanged again", tags) {
    std::vector<run_token_key> asked;
    time_point now{};
    bool refuse = false;
    run_token_cache cache(
        counting_minter{&asked, &now, seconds(300), &refuse}, 16, seconds(60), [&] { return now; });

    CHECK(cache.token_for(key_of("g1", "r1"), "t1") == "g1/r1#1");

    // More than the margin is left, so the held token is used.
    now += seconds(239);
    CHECK(cache.token_for(key_of("g1", "r1"), "t1") == "g1/r1#1");
    CHECK(asked.size() == 1);

    // Exactly the margin is left, so it is exchanged again.
    now += seconds(1);
    CHECK(cache.token_for(key_of("g1", "r1"), "t1") == "g1/r1#2");
    CHECK(asked.size() == 2);
}

TEST_CASE("a refused exchange holds nothing", tags) {
    std::vector<run_token_key> asked;
    time_point now{};
    bool refuse = true;
    run_token_cache cache(
        counting_minter{&asked, &now, seconds(300), &refuse}, 16, seconds(60), [&] { return now; });

    CHECK(cache.token_for(key_of("g1", "r1"), "t1").empty());
    CHECK(cache.size() == 0);

    refuse = false;
    CHECK(cache.token_for(key_of("g1", "r1"), "t1") == "g1/r1#2");
    CHECK(cache.size() == 1);
}

TEST_CASE("invalidate makes the next call exchange again", tags) {
    std::vector<run_token_key> asked;
    time_point now{};
    bool refuse = false;
    run_token_cache cache(
        counting_minter{&asked, &now, seconds(300), &refuse}, 16, seconds(60), [&] { return now; });

    CHECK(cache.token_for(key_of("g1", "r1"), "t1") == "g1/r1#1");
    cache.invalidate(key_of("g1", "r1"));
    CHECK(cache.size() == 0);
    CHECK(cache.token_for(key_of("g1", "r1"), "t1") == "g1/r1#2");
}

TEST_CASE("evict_run drops every entry of one run", tags) {
    std::vector<run_token_key> asked;
    time_point now{};
    bool refuse = false;
    run_token_cache cache(
        counting_minter{&asked, &now, seconds(300), &refuse}, 16, seconds(60), [&] { return now; });

    cache.token_for(key_of("g1", "r1"), "t1");
    cache.token_for(key_of("g2", "r1"), "t2");
    cache.token_for(key_of("g1", "r2"), "t1");
    REQUIRE(cache.size() == 3);

    cache.evict_run("r1");
    CHECK(cache.size() == 1);

    cache.token_for(key_of("g1", "r1"), "t1");
    CHECK(cache.size() == 2);
}

TEST_CASE("the cache is bounded and drops the least recently used", tags) {
    std::vector<run_token_key> asked;
    time_point now{};
    bool refuse = false;
    run_token_cache cache(
        counting_minter{&asked, &now, seconds(300), &refuse}, 2, seconds(60), [&] { return now; });

    cache.token_for(key_of("g1", "r1"), "t1");
    cache.token_for(key_of("g2", "r1"), "t1");
    // Touching g1 makes g2 the least recently used, so g3 evicts it.
    cache.token_for(key_of("g1", "r1"), "t1");
    cache.token_for(key_of("g3", "r1"), "t1");
    CHECK(cache.size() == 2);

    // g2 was dropped, so it is exchanged again; g3 is still held.
    CHECK(cache.token_for(key_of("g2", "r1"), "t1") == "g2/r1#4");
    CHECK(asked.size() == 4);
    CHECK(cache.token_for(key_of("g3", "r1"), "t1") == "g3/r1#3");
}

TEST_CASE("two grants and two runs hold their own tokens", tags) {
    std::vector<run_token_key> asked;
    time_point now{};
    bool refuse = false;
    run_token_cache cache(
        counting_minter{&asked, &now, seconds(300), &refuse}, 16, seconds(60), [&] { return now; });

    CHECK(cache.token_for(key_of("g1", "r1"), "t1") == "g1/r1#1");
    CHECK(cache.token_for(key_of("g2", "r1"), "t1") == "g2/r1#2");
    CHECK(cache.token_for(key_of("g1", "r2"), "t1") == "g1/r2#3");
    CHECK(cache.size() == 3);
}
