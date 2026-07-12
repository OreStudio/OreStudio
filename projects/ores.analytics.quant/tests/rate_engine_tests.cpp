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
#include "ores.analytics.quant/domain/driver_quote.hpp"
#include "ores.analytics.quant/domain/rate_status.hpp"
#include "ores.analytics.quant/domain/staleness_policy.hpp"
#include "ores.analytics.quant/service/rate_engine.hpp"
#include "ores.analytics.quant/service/topology_builder.hpp"
#include <atomic>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <limits>
#include <thread>
#include <vector>

using ores::analytics::quant::domain::ccy_pair_input;
using ores::analytics::quant::domain::driver_quote;
using ores::analytics::quant::domain::rate_status;
using ores::analytics::quant::domain::staleness_policy;
using ores::analytics::quant::service::rate_engine;
using ores::analytics::quant::service::topology_builder;

namespace {

auto epoch(int seconds) {
    return std::chrono::system_clock::time_point{} + std::chrono::seconds(seconds);
}

} // namespace

TEST_CASE("a direct driver rate is served back unchanged", "[rate_engine]") {
    const std::vector<ccy_pair_input> pairs = {{"EUR", "USD", true}};
    auto topology = topology_builder::build(pairs, "USD", {"EUR"});
    rate_engine engine(std::move(topology), staleness_policy{std::chrono::minutes(5)}, epoch(1000));

    engine.update(driver_quote{"EUR", "USD", 1.10, epoch(1000)});
    const auto result = engine.rate("EUR", "USD", epoch(1000));

    CHECK(result.status == rate_status::fresh);
    CHECK(result.rate == Catch::Approx(1.10));
}

TEST_CASE("a derived rate triangulates through the pivot", "[rate_engine]") {
    const std::vector<ccy_pair_input> pairs = {
        {"EUR", "USD", true},
        {"USD", "JPY", true},
    };
    auto topology = topology_builder::build(pairs, "USD", {"EUR", "JPY"});
    rate_engine engine(std::move(topology), staleness_policy{std::chrono::minutes(5)}, epoch(1000));

    engine.update(driver_quote{"EUR", "USD", 1.10, epoch(1000)});
    engine.update(driver_quote{"USD", "JPY", 150.0, epoch(1000)});

    // 1 EUR = 1.10 USD = 1.10 * 150 JPY
    const auto result = engine.rate("EUR", "JPY", epoch(1000));
    CHECK(result.status == rate_status::fresh);
    CHECK(result.rate == Catch::Approx(1.10 * 150.0));
}

TEST_CASE("an unseeded pair is unavailable until both drivers have ticked", "[rate_engine]") {
    const std::vector<ccy_pair_input> pairs = {
        {"EUR", "USD", true},
        {"USD", "JPY", true},
    };
    auto topology = topology_builder::build(pairs, "USD", {"EUR", "JPY"});
    rate_engine engine(std::move(topology), staleness_policy{std::chrono::minutes(5)}, epoch(1000));

    CHECK(engine.rate("EUR", "JPY", epoch(1000)).status == rate_status::unavailable);

    engine.update(driver_quote{"EUR", "USD", 1.10, epoch(1000)});
    // JPY leg still unseeded.
    CHECK(engine.rate("EUR", "JPY", epoch(1000)).status == rate_status::unavailable);

    engine.update(driver_quote{"USD", "JPY", 150.0, epoch(1000)});
    CHECK(engine.rate("EUR", "JPY", epoch(1000)).status == rate_status::fresh);
}

TEST_CASE("staleness propagates from the oldest contributing driver", "[rate_engine]") {
    const std::vector<ccy_pair_input> pairs = {
        {"EUR", "USD", true},
        {"USD", "JPY", true},
    };
    auto topology = topology_builder::build(pairs, "USD", {"EUR", "JPY"});
    const staleness_policy policy{std::chrono::seconds(60)};
    rate_engine engine(std::move(topology), policy, epoch(0));

    engine.update(driver_quote{"EUR", "USD", 1.10, epoch(0)});    // fresh at t=0
    engine.update(driver_quote{"USD", "JPY", 150.0, epoch(100)}); // fresh at t=100

    // EUR/JPY depends on the EUR/USD tick from t=0 -- 150s old at t=150,
    // beyond the 60s policy, so the whole derived rate is stale even
    // though the USD/JPY leg is fresh on its own.
    const auto result = engine.rate("EUR", "JPY", epoch(150));
    CHECK(result.status == rate_status::stale);
    CHECK(result.as_of == epoch(0));
}

TEST_CASE("updating one edge only recomputes its own subtree", "[rate_engine]") {
    const std::vector<ccy_pair_input> pairs = {
        {"EUR", "USD", true},
        {"USD", "JPY", true},
        {"GBP", "USD", true},
    };
    auto topology = topology_builder::build(pairs, "USD", {"EUR", "JPY", "GBP"});
    rate_engine engine(std::move(topology), staleness_policy{std::chrono::minutes(5)}, epoch(0));

    engine.update(driver_quote{"EUR", "USD", 1.10, epoch(0)});
    engine.update(driver_quote{"USD", "JPY", 150.0, epoch(0)});
    engine.update(driver_quote{"GBP", "USD", 1.25, epoch(0)});

    const auto before = engine.rate("GBP", "JPY", epoch(0));
    REQUIRE(before.status == rate_status::fresh);

    // Re-tick only the EUR leg; GBP/JPY (an unrelated subtree) must be
    // unaffected.
    engine.update(driver_quote{"EUR", "USD", 1.12, epoch(1)});
    const auto after = engine.rate("GBP", "JPY", epoch(0));
    CHECK(after.rate == Catch::Approx(before.rate));

    const auto eur_after = engine.rate("EUR", "USD", epoch(1));
    CHECK(eur_after.rate == Catch::Approx(1.12));
}

TEST_CASE("rates() batches a snapshot load across many pairs", "[rate_engine]") {
    const std::vector<ccy_pair_input> pairs = {
        {"EUR", "USD", true},
        {"USD", "JPY", true},
        {"GBP", "USD", true},
    };
    auto topology = topology_builder::build(pairs, "USD", {"EUR", "JPY", "GBP"});
    rate_engine engine(std::move(topology), staleness_policy{std::chrono::minutes(5)}, epoch(0));

    engine.update(driver_quote{"EUR", "USD", 1.10, epoch(0)});
    engine.update(driver_quote{"USD", "JPY", 150.0, epoch(0)});
    engine.update(driver_quote{"GBP", "USD", 1.25, epoch(0)});

    const auto results =
        engine.rates({{"EUR", "USD"}, {"EUR", "JPY"}, {"GBP", "JPY"}, {"USD", "USD"}}, epoch(0));

    REQUIRE(results.size() == 4);
    for (const auto& r : results)
        CHECK(r.status == rate_status::fresh);
    CHECK(results[3].rate == Catch::Approx(1.0)); // USD/USD is always identity via the pivot
}

TEST_CASE("update rejects an unknown currency", "[rate_engine]") {
    const std::vector<ccy_pair_input> pairs = {{"EUR", "USD", true}};
    auto topology = topology_builder::build(pairs, "USD", {"EUR"});
    rate_engine engine(std::move(topology), staleness_policy{std::chrono::minutes(5)}, epoch(0));

    CHECK_THROWS_AS(engine.update(driver_quote{"GBP", "USD", 1.25, epoch(0)}),
                    std::invalid_argument);
}

TEST_CASE("update rejects two known but unconnected currencies -- not an edge of this topology",
          "[rate_engine]") {
    const std::vector<ccy_pair_input> pairs = {
        {"EUR", "USD", true},
        {"USD", "JPY", true},
    };
    auto topology = topology_builder::build(pairs, "USD", {"EUR", "JPY"});
    rate_engine engine(std::move(topology), staleness_policy{std::chrono::minutes(5)}, epoch(0));

    // EUR and JPY are both known currencies, but neither is the other's
    // parent in the tree (both hang off USD) -- not a real edge.
    CHECK_THROWS_AS(engine.update(driver_quote{"EUR", "JPY", 130.0, epoch(0)}),
                    std::invalid_argument);
}

TEST_CASE("update rejects a self-referencing pair on the pivot currency", "[rate_engine]") {
    const std::vector<ccy_pair_input> pairs = {{"EUR", "USD", true}};
    auto topology = topology_builder::build(pairs, "USD", {"EUR"});
    rate_engine engine(std::move(topology), staleness_policy{std::chrono::minutes(5)}, epoch(0));

    // Without the base_id == quote_id guard, this would trivially satisfy
    // parent(pivot) == pivot and corrupt the pivot's log_rate.
    CHECK_THROWS_AS(engine.update(driver_quote{"USD", "USD", 1.0, epoch(0)}),
                    std::invalid_argument);

    // The pivot's own rate must still be exactly 1.0 afterwards -- the
    // rejected update must not have mutated any state.
    const auto result = engine.rate("USD", "USD", epoch(0));
    CHECK(result.rate == Catch::Approx(1.0));
}

TEST_CASE("update rejects a non-finite or non-positive rate", "[rate_engine]") {
    const std::vector<ccy_pair_input> pairs = {{"EUR", "USD", true}};
    auto topology = topology_builder::build(pairs, "USD", {"EUR"});
    rate_engine engine(std::move(topology), staleness_policy{std::chrono::minutes(5)}, epoch(0));

    CHECK_THROWS_AS(engine.update(driver_quote{"EUR", "USD", 0.0, epoch(0)}),
                    std::invalid_argument);
    CHECK_THROWS_AS(engine.update(driver_quote{"EUR", "USD", -1.1, epoch(0)}),
                    std::invalid_argument);
    CHECK_THROWS_AS(engine.update(driver_quote{
                        "EUR", "USD", std::numeric_limits<double>::quiet_NaN(), epoch(0)}),
                    std::invalid_argument);
    CHECK_THROWS_AS(engine.update(driver_quote{
                        "EUR", "USD", std::numeric_limits<double>::infinity(), epoch(0)}),
                    std::invalid_argument);
}

TEST_CASE("concurrent updates and batched reads never crash or hang", "[rate_engine][thread]") {
    const std::vector<ccy_pair_input> pairs = {
        {"EUR", "USD", true},
        {"USD", "JPY", true},
        {"GBP", "USD", true},
    };
    auto topology = topology_builder::build(pairs, "USD", {"EUR", "JPY", "GBP"});
    rate_engine engine(std::move(topology), staleness_policy{std::chrono::minutes(5)}, epoch(0));

    engine.update(driver_quote{"EUR", "USD", 1.10, epoch(0)});
    engine.update(driver_quote{"USD", "JPY", 150.0, epoch(0)});
    engine.update(driver_quote{"GBP", "USD", 1.25, epoch(0)});

    std::atomic<bool> stop{false};
    std::thread writer([&] {
        double rate = 1.10;
        while (!stop.load()) {
            rate += 0.0001;
            engine.update(driver_quote{"EUR", "USD", rate, epoch(0)});
        }
    });

    std::vector<std::thread> readers;
    for (int i = 0; i < 4; ++i) {
        readers.emplace_back([&] {
            for (int j = 0; j < 2000; ++j) {
                const auto results =
                    engine.rates({{"EUR", "USD"}, {"EUR", "JPY"}, {"GBP", "JPY"}}, epoch(0));
                for (const auto& r : results) {
                    REQUIRE(r.status == rate_status::fresh);
                    REQUIRE(r.rate > 0.0);
                }
            }
        });
    }
    for (auto& t : readers)
        t.join();
    stop.store(true);
    writer.join();
}
