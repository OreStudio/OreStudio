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
#include "ores.analytics.quant/domain/arrival_law.hpp"
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <stdexcept>
#include <vector>

using ores::analytics::quant::domain::arrival_law;
using ores::analytics::quant::domain::arrival_spec;
using ores::analytics::quant::domain::draw_gap;
using ores::analytics::quant::domain::hawkes_law;
using ores::analytics::quant::domain::make_arrival_law;
using ores::analytics::quant::domain::periodic_law;
using ores::analytics::quant::domain::poisson_law;
using ores::analytics::quant::domain::record_event;
using ores::analytics::quant::math::draw_purpose;
using ores::analytics::quant::math::open_uniform;
using ores::analytics::quant::math::rng_stream;
using ores::analytics::quant::math::stream_key;

namespace {

rng_stream stream_for(std::uint64_t seed) {
    return rng_stream(stream_key{.run_seed = seed, .series = 1, .purpose = draw_purpose::arrival});
}

arrival_spec spec(const char* mode, double per_second, double n = 0.0, double decay = 1.0) {
    return arrival_spec{
        .mode = mode, .ticks_per_hour = per_second * 3600.0, .branching_ratio = n, .decay = decay};
}

struct moments {
    double mean;
    double variance;
};

moments moments_of(const std::vector<double>& xs) {
    double total = 0.0;
    for (const double x : xs)
        total += x;
    const double mean = total / static_cast<double>(xs.size());
    double squared = 0.0;
    for (const double x : xs)
        squared += (x - mean) * (x - mean);
    return {mean, squared / static_cast<double>(xs.size() - 1)};
}

/// The Kolmogorov-Smirnov distance of a sample from a distribution function.
template <class Cdf>
double ks_distance(std::vector<double> xs, Cdf cdf) {
    std::ranges::sort(xs);
    const auto n = static_cast<double>(xs.size());
    double distance = 0.0;
    for (std::size_t i = 0; i < xs.size(); ++i) {
        const double f = cdf(xs[i]);
        distance = std::max(
            {distance, f - static_cast<double>(i) / n, static_cast<double>(i + 1) / n - f});
    }
    return distance;
}

double exponential_cdf(double x) {
    return 1.0 - std::exp(-x);
}

/// The distance a sample of n from the right law stays under 99% of the time.
double ks_bound(std::size_t n) {
    return 1.63 / std::sqrt(static_cast<double>(n));
}

/// Draw a stream of events and return each gap's compensator: the integral of
/// the intensity over the gap. For the right law they are Exp(1).
std::vector<double> compensated_gaps(arrival_law law, rng_stream& stream, std::size_t events) {
    std::vector<double> out;
    for (std::size_t i = 0; i < events; ++i) {
        const double gap = draw_gap(law, stream);
        if (const auto* p = std::get_if<poisson_law>(&law)) {
            out.push_back(p->rate * gap);
        } else {
            const auto& h = std::get<hawkes_law>(law);
            out.push_back(h.baseline * gap + h.excess * (1.0 - std::exp(-h.decay * gap)) / h.decay);
        }
        record_event(law, gap);
    }
    return out;
}

}

TEST_CASE("a periodic law starts at the phase it draws and is exactly periodic after it",
          "[arrival_law]") {
    auto stream = stream_for(5);
    auto twin = stream_for(5);
    auto law = make_arrival_law(spec("periodic", 4.0), stream);

    const double phase = open_uniform(twin);
    CHECK(std::get<periodic_law>(law).offset == phase);

    double gap = draw_gap(law, stream);
    CHECK(gap == Catch::Approx(phase * 0.25).epsilon(1e-15));
    record_event(law, gap);
    for (int i = 0; i < 3; ++i) {
        gap = draw_gap(law, stream);
        CHECK(gap == Catch::Approx(0.25).epsilon(1e-15));
        record_event(law, gap);
    }
}

TEST_CASE("periodic phases are uniform across series", "[arrival_law]") {
    constexpr std::size_t n = 20000;
    std::vector<double> phases;
    for (std::size_t seed = 0; seed < n; ++seed) {
        auto stream = stream_for(seed);
        phases.push_back(
            std::get<periodic_law>(make_arrival_law(spec("periodic", 1.0), stream)).offset);
    }

    const auto m = moments_of(phases);
    CHECK(m.mean == Catch::Approx(0.5).margin(0.01));
    CHECK(m.variance == Catch::Approx(1.0 / 12.0).margin(0.003));
    CHECK(ks_distance(phases, [](double x) { return x; }) < ks_bound(n));
}

TEST_CASE("a poisson gap is the exponential of the stream's next draw", "[arrival_law]") {
    auto stream = stream_for(9);
    auto twin = stream_for(9);
    auto law = make_arrival_law(spec("poisson", 8.0), stream);

    for (int i = 0; i < 5; ++i) {
        const double gap = draw_gap(law, stream);
        CHECK(gap == Catch::Approx(-std::log(open_uniform(twin)) / 8.0).epsilon(1e-15));
        record_event(law, gap);
    }
}

TEST_CASE("poisson gaps have the exponential's mean, variance and shape", "[arrival_law]") {
    constexpr std::size_t n = 100000;
    auto stream = stream_for(21);
    const auto law = make_arrival_law(spec("poisson", 1000.0), stream);

    std::vector<double> gaps;
    for (std::size_t i = 0; i < n; ++i)
        gaps.push_back(draw_gap(law, stream));
    const auto m = moments_of(gaps);
    CHECK(m.mean == Catch::Approx(0.001).epsilon(0.02));
    CHECK(m.variance == Catch::Approx(0.000001).epsilon(0.05));

    auto rescaled = stream_for(21);
    CHECK(ks_distance(compensated_gaps(law, rescaled, n), exponential_cdf) < ks_bound(n));
}

TEST_CASE("a hawkes gap is the earlier of its baseline and its excess arrival", "[arrival_law]") {
    auto stream = stream_for(13);
    auto twin = stream_for(13);
    const auto law = make_arrival_law(spec("hawkes", 10.0, 0.5, 4.0), stream);
    const auto& h = std::get<hawkes_law>(law);

    const double baseline_gap = -std::log(open_uniform(twin)) / h.baseline;
    const double d = 1.0 + h.decay * std::log(open_uniform(twin)) / h.excess;
    const double excess_gap =
        d > 0.0 ? -std::log(d) / h.decay : std::numeric_limits<double>::infinity();

    CHECK(draw_gap(law, stream) ==
          Catch::Approx(std::min(baseline_gap, excess_gap)).epsilon(1e-15));
}

TEST_CASE("drawing a gap without an event leaves a hawkes law as it was", "[arrival_law]") {
    // A scheduler that draws a gap and then drops it, as a refresh would, must
    // not move the law: the excess changes only when an event happens.
    auto stream = stream_for(3);
    const auto law = make_arrival_law(spec("hawkes", 10.0, 0.6, 5.0), stream);
    const auto before = std::get<hawkes_law>(law);

    for (int i = 0; i < 100; ++i)
        draw_gap(law, stream);

    const auto& after = std::get<hawkes_law>(law);
    CHECK(after.baseline == before.baseline);
    CHECK(after.jump == before.jump);
    CHECK(after.decay == before.decay);
    CHECK(after.excess == before.excess);
}

TEST_CASE("an event raises a hawkes excess by the jump after its fade", "[arrival_law]") {
    auto stream = stream_for(4);
    auto law = make_arrival_law(spec("hawkes", 10.0, 0.5, 4.0), stream);
    const double excess = std::get<hawkes_law>(law).excess;

    record_event(law, 0.25);
    CHECK(std::get<hawkes_law>(law).excess == Catch::Approx(excess * std::exp(-1.0) + 2.0));
}

TEST_CASE("a hawkes law starts at its stationary rate", "[arrival_law]") {
    // Rate ten, n = 0.8, decay five. Started at the baseline alone, the first
    // operational second would hold about 4.9 events on average; started at the
    // stationary excess it holds ten. The count over a second has a standard
    // deviation near 16, so 4000 series put the mean within about a quarter.
    const auto hawkes = spec("hawkes", 10.0, 0.8, 5.0);
    double events = 0.0;
    constexpr std::size_t series = 4000;
    for (std::size_t seed = 0; seed < series; ++seed) {
        auto stream = stream_for(seed);
        auto law = make_arrival_law(hawkes, stream);
        CHECK(std::get<hawkes_law>(law).excess == Catch::Approx(8.0));
        for (double t = 0.0;;) {
            const double gap = draw_gap(law, stream);
            t += gap;
            if (t >= 1.0)
                break;
            events += 1.0;
            record_event(law, gap);
        }
    }
    CHECK(events / series == Catch::Approx(10.0).margin(1.0));
}

TEST_CASE("hawkes gaps settle at the mean rate and rescale to Exp(1)", "[arrival_law]") {
    constexpr std::size_t n = 200000;
    auto stream = stream_for(17);
    auto law = make_arrival_law(spec("hawkes", 100.0, 0.5, 10.0), stream);
    const auto start = law;

    double total = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
        const double gap = draw_gap(law, stream);
        total += gap;
        record_event(law, gap);
    }
    CHECK(total / n == Catch::Approx(0.01).epsilon(0.03));

    auto rescaled = stream_for(17);
    const auto compensated = compensated_gaps(start, rescaled, n);
    const auto m = moments_of(compensated);
    CHECK(m.mean == Catch::Approx(1.0).epsilon(0.02));
    CHECK(m.variance == Catch::Approx(1.0).epsilon(0.05));
    CHECK(ks_distance(compensated, exponential_cdf) < ks_bound(n));
}

TEST_CASE("make_arrival_law refuses what cannot work", "[arrival_law]") {
    auto stream = stream_for(1);
    CHECK_THROWS_AS(make_arrival_law(spec("hawkes", 10.0, 1.0, 1.0), stream),
                    std::invalid_argument);
    CHECK_THROWS_AS(make_arrival_law(spec("poisson", 0.0), stream), std::invalid_argument);
    CHECK_THROWS_AS(make_arrival_law(spec("bursty", 1.0), stream), std::invalid_argument);
}

TEST_CASE("the compensator integrates each law's intensity over a gap", "[arrival_law]") {
    auto stream = stream_for(8);
    const auto hawkes = make_arrival_law(spec("hawkes", 10.0, 0.5, 4.0), stream);
    const auto& h = std::get<hawkes_law>(hawkes);
    CHECK(ores::analytics::quant::domain::compensator(hawkes, 0.3) ==
          Catch::Approx(h.baseline * 0.3 + h.excess * (1.0 - std::exp(-h.decay * 0.3)) / h.decay));
    CHECK(ores::analytics::quant::domain::intensity_after(hawkes, 0.0) ==
          Catch::Approx(h.baseline + h.excess));

    const auto poisson = make_arrival_law(spec("poisson", 8.0), stream);
    CHECK(ores::analytics::quant::domain::compensator(poisson, 0.25) == Catch::Approx(2.0));
    CHECK(ores::analytics::quant::domain::intensity_after(poisson, 3.0) == Catch::Approx(8.0));
}
