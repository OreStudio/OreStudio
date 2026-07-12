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
#include "ores.analytics.quant/service/risk_recentering.hpp"
#include "ores.analytics.quant/service/topology_builder.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

using ores::analytics::quant::domain::ccy_pair_input;
using ores::analytics::quant::domain::driver_quote;
using ores::analytics::quant::domain::rate_status;
using ores::analytics::quant::domain::staleness_policy;
using ores::analytics::quant::service::rate_engine;
using ores::analytics::quant::service::recenter;
using ores::analytics::quant::service::topology_builder;

namespace {

auto epoch(int seconds) {
    return std::chrono::system_clock::time_point{} + std::chrono::seconds(seconds);
}

rate_engine build_usd_pivot_engine() {
    const std::vector<ccy_pair_input> pairs = {
        {"EUR", "USD", true},
        {"USD", "JPY", true},
        {"GBP", "USD", true},
    };
    auto topology = topology_builder::build(pairs, "USD", {"EUR", "JPY", "GBP"});
    rate_engine engine(std::move(topology), staleness_policy{std::chrono::minutes(5)});
    engine.update(driver_quote{"EUR", "USD", 1.10, epoch(0)});
    engine.update(driver_quote{"USD", "JPY", 150.0, epoch(0)});
    engine.update(driver_quote{"GBP", "USD", 1.25, epoch(0)});
    return engine;
}

} // namespace

TEST_CASE("recentering preserves every rate's current value", "[risk_recentering]") {
    const auto source = build_usd_pivot_engine();

    // Recenter on GBP: everything must now route directly through GBP,
    // but the numeric rates against GBP must match the source engine's
    // triangulated values exactly.
    const auto expected_eur_gbp = source.rate("EUR", "GBP", epoch(0));
    const auto expected_jpy_gbp = source.rate("JPY", "GBP", epoch(0));
    const auto expected_usd_gbp = source.rate("USD", "GBP", epoch(0));
    REQUIRE(expected_eur_gbp.status == rate_status::fresh);
    REQUIRE(expected_jpy_gbp.status == rate_status::fresh);
    REQUIRE(expected_usd_gbp.status == rate_status::fresh);

    const auto recentred =
        recenter(source, "GBP", staleness_policy{std::chrono::minutes(5)}, epoch(0));

    CHECK(recentred.rate("EUR", "GBP", epoch(0)).rate == Catch::Approx(expected_eur_gbp.rate));
    CHECK(recentred.rate("JPY", "GBP", epoch(0)).rate == Catch::Approx(expected_jpy_gbp.rate));
    CHECK(recentred.rate("USD", "GBP", epoch(0)).rate == Catch::Approx(expected_usd_gbp.rate));
}

TEST_CASE("recentering keeps the original as_of, not the recentering time", "[risk_recentering]") {
    const std::vector<ccy_pair_input> pairs = {
        {"EUR", "USD", true},
        {"USD", "JPY", true},
    };
    auto topology = topology_builder::build(pairs, "USD", {"EUR", "JPY"});
    rate_engine source(std::move(topology), staleness_policy{std::chrono::minutes(5)});
    source.update(driver_quote{"EUR", "USD", 1.10, epoch(0)});
    source.update(driver_quote{"USD", "JPY", 150.0, epoch(50)});

    const auto recentred =
        recenter(source, "USD", staleness_policy{std::chrono::minutes(5)}, epoch(1000));

    // Recentering happens at t=1000, long after the underlying ticks --
    // the recentred engine must report the *original* as_of, not t=1000,
    // so its own staleness accounting stays honest.
    CHECK(recentred.rate("EUR", "USD", epoch(1000)).as_of == epoch(0));
    CHECK(recentred.rate("USD", "JPY", epoch(1000)).as_of == epoch(50));
}

TEST_CASE("bumping one leg of a recentred matrix leaves every other leg unchanged",
          "[risk_recentering]") {
    const auto source = build_usd_pivot_engine();
    auto recentred = recenter(source, "USD", staleness_policy{std::chrono::minutes(5)}, epoch(0));

    const auto jpy_before = recentred.rate("USD", "JPY", epoch(0));
    const auto gbp_before = recentred.rate("USD", "GBP", epoch(0));

    // In the un-recentred source, USD is already the pivot, so this test
    // is only meaningful once recentred is genuinely star-shaped -- bump
    // the EUR leg and confirm JPY/GBP (unrelated legs off the same star
    // centre) are bit-for-bit unaffected.
    recentred.update(driver_quote{"USD", "EUR", 1.0 / 1.12, epoch(1)});

    CHECK(recentred.rate("USD", "JPY", epoch(0)).rate == Catch::Approx(jpy_before.rate));
    CHECK(recentred.rate("USD", "GBP", epoch(0)).rate == Catch::Approx(gbp_before.rate));
}

TEST_CASE("an unavailable rate in the source is left unseeded after recentering",
          "[risk_recentering]") {
    const std::vector<ccy_pair_input> pairs = {
        {"EUR", "USD", true},
        {"USD", "JPY", true},
    };
    auto topology = topology_builder::build(pairs, "USD", {"EUR", "JPY"});
    rate_engine source(std::move(topology), staleness_policy{std::chrono::minutes(5)});
    source.update(driver_quote{"EUR", "USD", 1.10, epoch(0)});
    // JPY never ticked -- unavailable in the source.

    const auto recentred =
        recenter(source, "USD", staleness_policy{std::chrono::minutes(5)}, epoch(0));

    CHECK(recentred.rate("USD", "EUR", epoch(0)).status == rate_status::fresh);
    CHECK(recentred.rate("USD", "JPY", epoch(0)).status == rate_status::unavailable);
}

TEST_CASE("recentering on an unknown currency throws", "[risk_recentering]") {
    const auto source = build_usd_pivot_engine();
    CHECK_THROWS_AS(recenter(source, "XXX", staleness_policy{std::chrono::minutes(5)}, epoch(0)),
                    std::invalid_argument);
}
