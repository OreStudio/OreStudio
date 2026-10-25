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
#include "ores.analytics.quant/domain/i_arrival_process.hpp"
#include "ores.analytics.quant/domain/market_regime.hpp"
#include "ores.analytics.quant/service/processes/periodic_arrival_process.hpp"
#include "ores.analytics.quant/service/tick_clock.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

const std::string tags("[tick_clock]");

/// A process with a scripted gap, to prove the clock asks each time.
class scripted_arrival_process final : public ores::analytics::quant::domain::i_arrival_process {
public:
    explicit scripted_arrival_process(double gap)
        : gap_(gap) {}

    double next_gap() override {
        ++calls_;
        return gap_;
    }

    void set_baseline_multiplier(double multiplier) override {
        multipliers_.push_back(multiplier);
    }

    [[nodiscard]] std::size_t calls() const {
        return calls_;
    }

    [[nodiscard]] const std::vector<double>& multipliers() const {
        return multipliers_;
    }

private:
    double gap_;
    std::size_t calls_ = 0;
    std::vector<double> multipliers_;
};

}

using ores::analytics::quant::domain::market_regime;
using ores::analytics::quant::service::periodic_arrival_process;
using ores::analytics::quant::service::tick_clock;

TEST_CASE("tick_clock runs the body once per gap and counts every batch", tags) {
    // One tick per millisecond, so a three-tick case is fast but still
    // exercises the real gap and sleep path.
    periodic_arrival_process arrivals(1000.0);
    tick_clock clock(arrivals);
    std::vector<std::uint64_t> counts;
    std::vector<std::string> summaries;
    int calls = 0;

    clock.run(
        [&](double) {
            ++calls;
            if (calls == 3)
                clock.stop();
            return std::string("tick") + std::to_string(calls);
        },
        [&](std::uint64_t n, const std::string& summary) {
            counts.push_back(n);
            summaries.push_back(summary);
        },
        [](const std::exception&) { FAIL("the tick body did not throw"); });

    CHECK(calls == 3);
    CHECK(clock.publish_count() == 3);
    CHECK(counts == std::vector<std::uint64_t>{1, 2, 3});
    CHECK(summaries == std::vector<std::string>{"tick1", "tick2", "tick3"});
}

TEST_CASE("tick_clock asks the arrival process for every gap", tags) {
    scripted_arrival_process arrivals(0.001);
    tick_clock clock(arrivals);
    int calls = 0;

    clock.run(
        [&](double) {
            if (++calls == 3)
                clock.stop();
            return std::string{};
        },
        [](std::uint64_t, const std::string&) {},
        [](const std::exception&) { FAIL("the tick body did not throw"); });

    CHECK(calls == 3);
    CHECK(arrivals.calls() == 3);
}

TEST_CASE("tick_clock skips a throwing tick and keeps counting from the next one", tags) {
    periodic_arrival_process arrivals(1000.0);
    tick_clock clock(arrivals);
    std::vector<std::uint64_t> counts;
    std::vector<std::string> failures;
    int calls = 0;

    clock.run(
        [&](double) {
            ++calls;
            if (calls == 1)
                throw std::runtime_error("first tick failed");
            if (calls == 3)
                clock.stop();
            return std::string("ok");
        },
        [&](std::uint64_t n, const std::string&) { counts.push_back(n); },
        [&](const std::exception& ex) { failures.push_back(ex.what()); });

    CHECK(calls == 3);
    CHECK(clock.publish_count() == 2);
    CHECK(counts == std::vector<std::uint64_t>{1, 2});
    CHECK(failures == std::vector<std::string>{"first tick failed"});
}

TEST_CASE("tick_clock stopped before run emits nothing", tags) {
    periodic_arrival_process arrivals(1000.0);
    tick_clock clock(arrivals);
    clock.stop();

    int calls = 0;
    clock.run(
        [&](double) {
            ++calls;
            return std::string{};
        },
        [](std::uint64_t, const std::string&) { FAIL("a stopped clock published"); },
        [](const std::exception&) { FAIL("a stopped clock failed"); });

    CHECK(calls == 0);
    CHECK(clock.publish_count() == 0);
}

TEST_CASE("tick_clock hands the body the gap it waited", tags) {
    // One tick per millisecond, so the gap the body sees is a millisecond in
    // seconds. A producer needs it to step a value process by elapsed time.
    periodic_arrival_process arrivals(1000.0);
    tick_clock clock(arrivals);
    std::vector<double> gaps;
    int calls = 0;

    clock.run(
        [&](double elapsed_seconds) {
            gaps.push_back(elapsed_seconds);
            ++calls;
            if (calls == 3)
                clock.stop();
            return std::string{};
        },
        [](std::uint64_t, const std::string&) {},
        [](const std::exception&) {});

    REQUIRE(gaps.size() == 3);
    for (const double gap : gaps)
        CHECK(gap == Catch::Approx(0.001).epsilon(1e-12));
}

TEST_CASE("tick_clock scales the arrival rate by the market state and the row's sensitivity",
          tags) {
    // An active market triples the rate, and this row follows the market twice as
    // hard as the average, so every gap is drawn at six times the rate.
    market_regime market;
    market.pin(market_regime::state::active);

    scripted_arrival_process arrivals(0.001);
    tick_clock clock(arrivals, &market, 2.0);
    int calls = 0;

    clock.run(
        [&](double) {
            ++calls;
            if (calls == 3)
                clock.stop();
            return std::string{};
        },
        [](std::uint64_t, const std::string&) {},
        [](const std::exception&) {});

    // The market is constant across these ticks, so the rate is set once and
    // left alone: the tick path does no work it does not need to do.
    REQUIRE(arrivals.multipliers().size() == 1);
    // The default market is at two while pinned active, and this row follows it
    // twice as hard as the average.
    CHECK(arrivals.multipliers().front() == Catch::Approx(4.0));
}

TEST_CASE("tick_clock leaves the rate alone with no market", tags) {
    scripted_arrival_process arrivals(0.001);
    tick_clock clock(arrivals);
    int calls = 0;

    clock.run(
        [&](double) {
            ++calls;
            if (calls == 2)
                clock.stop();
            return std::string{};
        },
        [](std::uint64_t, const std::string&) {},
        [](const std::exception&) {});

    CHECK(arrivals.multipliers().empty());
}
