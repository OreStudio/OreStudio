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
#include "ores.http.api/net/rate_limiter.hpp"
#include "ores.http.api/net/router.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <string>
#include <vector>

using namespace std::chrono_literals;
using ores::http::net::http_server_options;
using ores::http::net::rate_decision;
using ores::http::net::rate_key;
using ores::http::net::rate_limiter;
using ores::http::net::rate_policies;
using ores::http::net::rate_policy;

namespace {

/// A clock the test moves by hand.
struct manual_clock final {
    std::chrono::steady_clock::time_point now{std::chrono::steady_clock::time_point{} + 1h};
    rate_limiter::clock reader() {
        return [this] {
            return now;
        };
    }
};

rate_policy per_minute(std::string name, std::uint32_t limit) {
    return rate_policy{std::move(name), limit, 60s, rate_key::client, "requests"};
}

ores::http::domain::route route_of(std::string pattern, bool auth, std::string policy = {}) {
    ores::http::domain::route r;
    r.pattern = std::move(pattern);
    r.requires_auth = auth;
    r.rate_policy = std::move(policy);
    return r;
}

}

TEST_CASE("a_caller_may_spend_the_whole_limit_at_once_and_no_more", "[rate_limiter]") {
    manual_clock clock;
    rate_limiter limiter(1024, clock.reader());
    const auto policy = per_minute("p", 3);
    const auto first = limiter.take(policy, "a");
    CHECK(first.allowed);
    CHECK(first.remaining == 2);
    CHECK(first.reset == 20s);
    CHECK(limiter.take(policy, "a").remaining == 1);
    const auto last = limiter.take(policy, "a");
    CHECK(last.allowed);
    CHECK(last.remaining == 0);
    CHECK(last.reset == 60s);
    const rate_decision refused = limiter.take(policy, "a");
    CHECK_FALSE(refused.allowed);
    CHECK(refused.remaining == 0);
    CHECK(refused.retry_after == 20s);
}

TEST_CASE("one_request_comes_back_every_interval", "[rate_limiter]") {
    manual_clock clock;
    rate_limiter limiter(1024, clock.reader());
    const auto policy = per_minute("p", 3);
    for (int i = 0; i < 3; ++i)
        CHECK(limiter.take(policy, "a").allowed);
    clock.now += 19s;
    const auto early = limiter.take(policy, "a");
    CHECK_FALSE(early.allowed);
    CHECK(early.retry_after == 1s);
    clock.now += 1s;
    CHECK(limiter.take(policy, "a").allowed);
    CHECK_FALSE(limiter.take(policy, "a").allowed);
}

TEST_CASE("callers_and_policies_have_budgets_of_their_own", "[rate_limiter]") {
    manual_clock clock;
    rate_limiter limiter(1024, clock.reader());
    const auto one = per_minute("one", 1);
    const auto other = per_minute("other", 1);
    CHECK(limiter.take(one, "a").allowed);
    CHECK_FALSE(limiter.take(one, "a").allowed);
    CHECK(limiter.take(one, "b").allowed);
    CHECK(limiter.take(other, "a").allowed);
    // The same name is the same budget, whichever route names it.
    CHECK_FALSE(limiter.take(per_minute("one", 1), "a").allowed);
}

TEST_CASE("a_full_bucket_is_swept_and_the_bound_is_kept", "[rate_limiter]") {
    manual_clock clock;
    rate_limiter limiter(16, clock.reader());
    const auto policy = per_minute("p", 2);
    CHECK(limiter.take(policy, "a").allowed);
    CHECK(limiter.take(policy, "b").allowed);
    CHECK(limiter.tracked() == 2);
    clock.now += 30s;
    CHECK(limiter.sweep() == 2);
    CHECK(limiter.tracked() == 0);
    for (int i = 0; i < 1000; ++i)
        CHECK(limiter.take(policy, "caller" + std::to_string(i)).allowed);
    CHECK(limiter.tracked() <= 16);
}

TEST_CASE("every_route_runs_under_a_policy", "[rate_limiter]") {
    http_server_options options;
    options.open_requests_per_minute = 7;
    options.authenticated_requests_per_minute = 9;
    options.credential_attempts_per_minute = 3;
    const auto policies = rate_policies::from(options);
    CHECK(policies.for_route(std::nullopt).name == "open");
    CHECK(policies.for_route(route_of("/open", false)).limit == 7);
    const auto& authenticated = policies.for_route(route_of("/mine", true));
    CHECK(authenticated.limit == 9);
    CHECK(authenticated.key == rate_key::account);
    const auto& credential =
        policies.for_route(route_of("/login", false, std::string(rate_policies::credential)));
    CHECK(credential.limit == 3);
    CHECK(credential.key == rate_key::client);
    CHECK(policies.unknown_policies({route_of("/a", false, "credential"), route_of("/b", false)})
              .empty());
    CHECK(policies.unknown_policies({route_of("/typo", false, "credentail")}) ==
          std::vector<std::string>{"/typo names the rate policy credentail"});
    options.credential_attempts_per_minute = 0;
    CHECK_THROWS_AS(rate_policies::from(options), std::invalid_argument);
}

TEST_CASE("a_route_names_its_policy_on_the_builder", "[rate_limiter]") {
    ores::http::net::router table;
    const auto built =
        table.post("/login")
            .auth_optional()
            .rate_limit(rate_policies::credential)
            .handler([](const ores::http::domain::http_request&)
                         -> boost::asio::awaitable<ores::http::domain::http_response> {
                co_return ores::http::domain::http_response::json("{}");
            })
            .build();
    CHECK(built.rate_policy == "credential");
}

TEST_CASE("a_refusal_states_the_wait_and_the_policy", "[rate_limiter]") {
    const auto policy = rate_policy{"credential", 10, 60s, rate_key::client, "sign-in attempts"};
    const auto answer = ores::http::net::rate_limited(policy, rate_decision{false, 0, 60s, 6s});
    CHECK(answer.status == ores::http::domain::http_status::too_many_requests);
    CHECK(answer.body.find("too-many-requests") != std::string::npos);
    CHECK(answer.body.find("Too many sign-in attempts. Wait a minute and try again.") !=
          std::string::npos);
    CHECK(answer.headers.at("Retry-After") == "6");
    CHECK(answer.headers.at("RateLimit-Policy") == "\"credential\";q=10;w=60");
    CHECK(answer.headers.at("RateLimit") == "\"credential\";r=0;t=60");
}
