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
#include "ores.http/routes/iam/agent_key_exchange.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <memory>

using ores::http::routes::iam::agent_key_cache;
using ores::http::routes::iam::agent_key_exchange;
using namespace std::chrono_literals;

namespace {

const auto agent_cache_start = std::chrono::system_clock::time_point(std::chrono::hours(1000));

agent_key_cache::entry five_minute_token(std::string token) {
    return {.token = std::move(token),
            .issued_at = agent_cache_start,
            .expires_at = agent_cache_start + 5min};
}

}

TEST_CASE("agent_key_cache_serves_a_token_while_a_fifth_of_its_life_is_left",
          "[agent_key_exchange]") {
    agent_key_cache cache;
    cache.put("hash-1", five_minute_token("t1"));

    CHECK(cache.find("hash-1", agent_cache_start + 3min) == std::optional<std::string>("t1"));
    // At four minutes exactly one fifth is left, which is not more than a fifth.
    CHECK_FALSE(cache.find("hash-1", agent_cache_start + 4min));
    // The stale entry was dropped, so even an earlier clock finds nothing.
    CHECK_FALSE(cache.find("hash-1", agent_cache_start));
    CHECK_FALSE(cache.find("hash-2", agent_cache_start));
}

TEST_CASE("agent_key_cache_clear_forgets_every_token", "[agent_key_exchange]") {
    agent_key_cache cache;
    cache.put("hash-1", five_minute_token("t1"));
    cache.put("hash-2", five_minute_token("t2"));
    cache.clear();
    CHECK_FALSE(cache.find("hash-1", agent_cache_start));
    CHECK_FALSE(cache.find("hash-2", agent_cache_start));
}

TEST_CASE("agent_key_exchange_leaves_a_value_that_is_not_a_key_to_the_verifier",
          "[agent_key_exchange]") {
    // The client is never used: a value that is not a key makes no request.
    ores::nats::service::nats_client unused;
    const auto exchange =
        agent_key_exchange::exchanger(unused, std::make_shared<agent_key_cache>());
    CHECK_FALSE(exchange("eyJhbGciOiJSUzI1NiJ9.e30.sig", "203.0.113.7"));
    CHECK_FALSE(exchange("", "203.0.113.7"));
}
