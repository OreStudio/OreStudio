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
#include "ores.analytics.quant/domain/process_parameter_validation.hpp"
#include "ores.analytics.quant/service/process_factory.hpp"
#include "ores.analytics.quant/service/processes/hawkes_arrival_process.hpp"
#include "ores.analytics.quant/service/processes/periodic_arrival_process.hpp"
#include "ores.analytics.quant/service/processes/poisson_arrival_process.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstdint>
#include <stdexcept>
#include <vector>

namespace {

const std::string tags("[arrival_process]");

/// The long-run mean of a gap, measured over enough draws to be tight.
double mean_gap(ores::analytics::quant::domain::i_arrival_process& process, std::size_t draws) {
    double total = 0.0;
    for (std::size_t i = 0; i < draws; ++i)
        total += process.next_gap();
    return total / static_cast<double>(draws);
}

/// Count the events of a stream in equal windows, for the Fano factor.
std::vector<double> counts_per_window(ores::analytics::quant::domain::i_arrival_process& process,
                                      std::size_t events,
                                      double window) {
    std::vector<double> counts;
    double window_end = window;
    double count = 0.0;
    double now = 0.0;

    for (std::size_t i = 0; i < events; ++i) {
        now += process.next_gap();
        while (now >= window_end) {
            counts.push_back(count);
            count = 0.0;
            window_end += window;
        }
        count += 1.0;
    }
    return counts;
}

/// Variance over mean of a count series. It is 1 for a Poisson stream and
/// 1/(1-n)^2 for a Hawkes stream.
double fano_factor(const std::vector<double>& counts) {
    double total = 0.0;
    for (double c : counts)
        total += c;
    const double mean = total / static_cast<double>(counts.size());

    double squared = 0.0;
    for (double c : counts)
        squared += (c - mean) * (c - mean);
    const double variance = squared / static_cast<double>(counts.size() - 1);

    return variance / mean;
}

}

using ores::analytics::quant::service::hawkes_arrival_process;
using ores::analytics::quant::service::periodic_arrival_process;
using ores::analytics::quant::service::poisson_arrival_process;

TEST_CASE("the periodic process returns the gap its mean rate asks for", tags) {
    periodic_arrival_process per_second(4.0);
    CHECK(per_second.next_gap() == Catch::Approx(0.25));
    CHECK(per_second.next_gap() == Catch::Approx(0.25));

    periodic_arrival_process every_two_seconds(0.5);
    CHECK(every_two_seconds.next_gap() == Catch::Approx(2.0));
}

TEST_CASE("the poisson process draws gaps at its mean rate", tags) {
    // 1000 arrivals a second, so a gap of one millisecond, and 200k draws put
    // the sample mean within a fraction of a percent of it.
    poisson_arrival_process process(1000.0, 7);
    CHECK(mean_gap(process, 200000) == Catch::Approx(0.001).epsilon(0.02));
}

TEST_CASE("the poisson counts are independent: the Fano factor is one", tags) {
    // Gaps that were not exponential would not give a Fano factor of one, so
    // this rules out a constant gap where the mean alone cannot.
    poisson_arrival_process process(10.0, 19);
    const auto counts = counts_per_window(process, 100000, 1.0);

    REQUIRE(counts.size() > 9000);
    CHECK(fano_factor(counts) == Catch::Approx(1.0).epsilon(0.15));
}

TEST_CASE("a hawkes stream settles at its long-run mean rate", tags) {
    // The long-run rate is the rate the caller asked for, mu / (1 - n), not the
    // baseline, which is half of it here.
    hawkes_arrival_process process(1000.0, 0.5, 10.0, 11);
    CHECK(process.baseline() == Catch::Approx(500.0));
    CHECK(process.jump() == Catch::Approx(5.0));

    CHECK(mean_gap(process, 200000) == Catch::Approx(0.001).epsilon(0.03));
}

TEST_CASE("a hawkes stream clusters: the Fano factor is one over one minus n squared", tags) {
    // Windows of a second are ten bursts long (1 / beta = 0.1 s), so the count
    // inside one window sees the clustering. For n = 0.5 the Fano factor is 4.
    hawkes_arrival_process process(10.0, 0.5, 10.0, 13);
    const auto counts = counts_per_window(process, 100000, 1.0);

    REQUIRE(counts.size() > 9000);
    CHECK(fano_factor(counts) == Catch::Approx(4.0).epsilon(0.15));
}

TEST_CASE("the rescaled gaps of a hawkes stream are exponential with mean one", tags) {
    // The time-rescaling test: integrate the intensity over each gap. For the
    // exponential kernel that integral is closed form, and the compensated
    // gaps must be Exp(1), whose mean and variance are both one.
    constexpr double mean_rate = 100.0;
    constexpr double branching_ratio = 0.5;
    constexpr double decay = 10.0;
    hawkes_arrival_process process(mean_rate, branching_ratio, decay, 17);

    const double baseline = process.baseline();
    const double jump = process.jump();

    constexpr std::size_t draws = 200000;
    double excess = 0.0;
    double total = 0.0;
    double squared = 0.0;

    for (std::size_t i = 0; i < draws; ++i) {
        const auto gap = process.next_gap();
        const auto increment = baseline * gap + excess * (1.0 - std::exp(-decay * gap)) / decay;
        total += increment;
        squared += increment * increment;
        excess = excess * std::exp(-decay * gap) + jump;
    }

    const auto mean = total / static_cast<double>(draws);
    const auto variance = squared / static_cast<double>(draws) - mean * mean;

    CHECK(mean == Catch::Approx(1.0).epsilon(0.05));
    CHECK(variance == Catch::Approx(1.0).epsilon(0.15));
}

TEST_CASE("a branching ratio of one or more is refused", tags) {
    CHECK_THROWS_AS(hawkes_arrival_process(10.0, 1.0, 10.0), std::invalid_argument);
    CHECK_THROWS_AS(hawkes_arrival_process(10.0, 1.5, 10.0), std::invalid_argument);
    CHECK_THROWS_AS(ores::analytics::quant::service::process_factory::make_arrival_process(
                        "hawkes", 3600.0, 1.0, 10.0),
                    std::invalid_argument);

    const auto refused = ores::analytics::quant::domain::validate_arrival_process_parameters(
        "hawkes", 3600.0, 1.0, 10.0);
    CHECK_FALSE(refused.valid);
    CHECK_FALSE(refused.message.empty());
}

TEST_CASE("the arrival rules refuse what cannot work", tags) {
    using ores::analytics::quant::domain::validate_arrival_process_parameters;

    CHECK_FALSE(validate_arrival_process_parameters("periodic", 0.0, 0.0, 1.0).valid);
    CHECK_FALSE(validate_arrival_process_parameters("poisson", -1.0, 0.0, 1.0).valid);
    CHECK_FALSE(validate_arrival_process_parameters("hawkes", 3600.0, 0.5, 0.0).valid);
    CHECK_FALSE(validate_arrival_process_parameters("hawkes", 3600.0, -0.1, 10.0).valid);
    CHECK_FALSE(validate_arrival_process_parameters("annual", 3600.0, 0.0, 1.0).valid);

    // The two non-clustering modes ignore the clustering numbers.
    CHECK(validate_arrival_process_parameters("periodic", 3600.0, 5.0, -1.0).valid);
    CHECK(validate_arrival_process_parameters("poisson", 3600.0, 5.0, -1.0).valid);
    CHECK(validate_arrival_process_parameters("hawkes", 3600.0, 0.5, 10.0).valid);
    CHECK(validate_arrival_process_parameters("hawkes", 3600.0, 0.0, 10.0).valid);
}

TEST_CASE("the factory converts ticks per hour to the process unit and refuses a bad mode", tags) {
    const auto hourly =
        ores::analytics::quant::service::process_factory::make_arrival_process("periodic", 3600.0);
    CHECK(hourly->next_gap() == Catch::Approx(1.0));

    const auto half_hourly =
        ores::analytics::quant::service::process_factory::make_arrival_process("periodic", 7200.0);
    CHECK(half_hourly->next_gap() == Catch::Approx(0.5));

    CHECK_THROWS_AS(
        ores::analytics::quant::service::process_factory::make_arrival_process("annual", 3600.0),
        std::invalid_argument);
    CHECK_THROWS_AS(
        ores::analytics::quant::service::process_factory::make_arrival_process("periodic", 0.0),
        std::invalid_argument);
}

TEST_CASE("the factory builds what a spec names", tags) {
    using ores::analytics::quant::domain::arrival_spec;
    using ores::analytics::quant::service::process_factory;

    const auto periodic = process_factory::make_arrival_process(
        arrival_spec{.mode = "periodic", .ticks_per_hour = 3600.0});
    CHECK(periodic->next_gap() == Catch::Approx(1.0));

    const auto hawkes = process_factory::make_arrival_process(arrival_spec{
        .mode = "hawkes", .ticks_per_hour = 3600.0, .branching_ratio = 0.5, .decay = 10.0});
    REQUIRE(hawkes);
    CHECK(hawkes->next_gap() > 0.0);

    CHECK_THROWS_AS(
        process_factory::make_arrival_process(arrival_spec{
            .mode = "hawkes", .ticks_per_hour = 3600.0, .branching_ratio = 1.0, .decay = 10.0}),
        std::invalid_argument);
}

TEST_CASE("a market-wide factor scales the baseline of every process", tags) {
    using ores::analytics::quant::service::hawkes_arrival_process;
    using ores::analytics::quant::service::periodic_arrival_process;
    using ores::analytics::quant::service::poisson_arrival_process;

    // The periodic gap is the reciprocal of the rate, so twice the rate is half
    // the gap.
    periodic_arrival_process periodic(2.0);
    REQUIRE(periodic.next_gap() == Catch::Approx(0.5));
    periodic.set_baseline_multiplier(2.0);
    CHECK(periodic.next_gap() == Catch::Approx(0.25));
    periodic.set_baseline_multiplier(1.0);
    CHECK(periodic.next_gap() == Catch::Approx(0.5));

    // The Poisson mean gap follows the same reciprocal, measured over enough
    // draws that the noise is small.
    poisson_arrival_process poisson(10.0, 7);
    double unscaled = 0.0;
    for (int i = 0; i < 2000; ++i)
        unscaled += poisson.next_gap();
    poisson.set_baseline_multiplier(4.0);
    double scaled = 0.0;
    for (int i = 0; i < 2000; ++i)
        scaled += poisson.next_gap();
    CHECK(scaled * 4.0 == Catch::Approx(unscaled).epsilon(0.1));

    // The Hawkes baseline moves and the branching ratio does not: the shape of a
    // burst is not something a busy market changes.
    hawkes_arrival_process hawkes(10.0, 0.5, 20.0);
    const double baseline = hawkes.baseline();
    const double jump = hawkes.jump();
    hawkes.set_baseline_multiplier(3.0);
    CHECK(hawkes.baseline() == Catch::Approx(baseline * 3.0));
    CHECK(hawkes.jump() == Catch::Approx(jump));

    CHECK_THROWS_AS(periodic.set_baseline_multiplier(0.0), std::invalid_argument);
    CHECK_THROWS_AS(poisson.set_baseline_multiplier(-1.0), std::invalid_argument);
    CHECK_THROWS_AS(hawkes.set_baseline_multiplier(0.0), std::invalid_argument);
}

TEST_CASE("a pinned market leaves the per-series law intact", tags) {
    using ores::analytics::quant::service::poisson_arrival_process;

    // A market that doubles the rate halves the mean gap and leaves the Fano
    // factor at one: the factor moves the rate, it does not change the process.
    poisson_arrival_process poisson(10.0, 3);
    double unscaled_sum = 0.0;
    for (int i = 0; i < 4000; ++i)
        unscaled_sum += poisson.next_gap();
    const double unscaled_mean = unscaled_sum / 4000.0;

    poisson.set_baseline_multiplier(2.0);
    double scaled_sum = 0.0;
    double scaled_square_sum = 0.0;
    for (int i = 0; i < 4000; ++i) {
        const double gap = poisson.next_gap();
        scaled_sum += gap;
        scaled_square_sum += gap * gap;
    }
    const double scaled_mean = scaled_sum / 4000.0;
    const double variance = scaled_square_sum / 4000.0 - scaled_mean * scaled_mean;
    const double fano = variance / (scaled_mean * scaled_mean);

    CHECK(scaled_mean == Catch::Approx(unscaled_mean / 2.0).epsilon(0.1));
    // An exponential gap has variance mean^2, so the Fano factor of the gaps is
    // one within sampling noise.
    CHECK(fano == Catch::Approx(1.0).epsilon(0.2));
}
