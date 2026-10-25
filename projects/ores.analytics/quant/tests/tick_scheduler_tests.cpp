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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using ores::analytics::quant::domain::i_arrival_process;
using ores::analytics::quant::domain::market_link;
using ores::analytics::quant::service::tick_scheduler;

namespace {

/* A series with a fixed gap and a record of the rates it was asked for. */
struct fixed_gap_process final : i_arrival_process {
    explicit fixed_gap_process(double gap)
        : gap_(gap) {}

    double next_gap() override {
        return gap_;
    }
    void set_baseline_multiplier(double multiplier) override {
        multipliers.push_back(multiplier);
    }

    double gap_;
    std::vector<double> multipliers;
};

}

TEST_CASE("the scheduler ticks the earliest series first", "[tick_scheduler]") {
    fixed_gap_process slow(0.5);
    fixed_gap_process fast(0.1);
    tick_scheduler scheduler;
    std::vector<std::string> order;

    scheduler.add(slow, {}, [&](double) {
        order.emplace_back("slow");
        return std::string{"slow"};
    });
    scheduler.add(fast, {}, [&](double) {
        order.emplace_back("fast");
        return std::string{"fast"};
    });

    REQUIRE(scheduler.size() == 2);
    for (int i = 0; i < 4; ++i)
        scheduler.step();

    // The fast series has the shorter gap, so it goes first and keeps going.
    REQUIRE(order.size() == 4);
    CHECK(order.front() == "fast");
    CHECK(order[1] == "fast");
}

TEST_CASE("the scheduler hands each series the gap it waited", "[tick_scheduler]") {
    fixed_gap_process process(0.25);
    tick_scheduler scheduler;
    std::vector<double> elapsed;

    scheduler.add(process, {}, [&](double seconds) {
        elapsed.push_back(seconds);
        return std::string{};
    });

    scheduler.step();
    scheduler.step();

    REQUIRE(elapsed.size() == 2);
    for (const double e : elapsed)
        CHECK(e == Catch::Approx(0.25));
}

TEST_CASE("the scheduler re-rates a series from its link", "[tick_scheduler]") {
    fixed_gap_process process(0.1);
    tick_scheduler scheduler;
    scheduler.add(process, market_link{.sensitivity = 2.0, .mean_rate = 1.0}, [](double) {
        return std::string{};
    });

    scheduler.step();
    REQUIRE_FALSE(process.multipliers.empty());
    // No factors, so the link's multiplier falls back to one and the sensitivity
    // is not applied to nothing.
    CHECK(process.multipliers.front() == Catch::Approx(1.0));
}

TEST_CASE("the scheduler survives a body that throws", "[tick_scheduler]") {
    fixed_gap_process process(0.1);
    tick_scheduler scheduler;
    int failures = 0;
    scheduler.set_hooks([](std::uint64_t, const std::string&) {},
                        [&](const std::exception&) { ++failures; });
    scheduler.add(
        process, {}, [](double) -> std::string { throw std::runtime_error("the body failed"); });

    CHECK(scheduler.step().has_value());
    CHECK(failures == 1);
    CHECK(scheduler.publish_count() == 0);
}

TEST_CASE("the scheduler does nothing with no series", "[tick_scheduler]") {
    tick_scheduler scheduler;
    CHECK_FALSE(scheduler.step().has_value());
    CHECK(scheduler.publish_count() == 0);
}
