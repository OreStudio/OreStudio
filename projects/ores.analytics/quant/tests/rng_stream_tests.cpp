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
#include "ores.analytics.quant/math/rng_stream.hpp"
#include <array>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstdint>
#include <random>
#include <set>
#include <vector>

using ores::analytics::quant::math::draw_purpose;
using ores::analytics::quant::math::philox4x32_10;
using ores::analytics::quant::math::rng_stream;
using ores::analytics::quant::math::series_key;
using ores::analytics::quant::math::stream_key;

namespace {

using block = std::array<std::uint32_t, 4>;

std::vector<std::uint64_t> take(rng_stream& stream, std::size_t n) {
    std::vector<std::uint64_t> out;
    for (std::size_t i = 0; i < n; ++i)
        out.push_back(stream());
    return out;
}

double uniform(std::uint64_t bits) {
    return static_cast<double>(bits >> 11) * 0x1.0p-53;
}

}

TEST_CASE("philox4x32_10 reproduces the Random123 known answers", "[rng_stream]") {
    STATIC_REQUIRE(philox4x32_10({0, 0, 0, 0}, {0, 0}) ==
                   block{0x6627e8d5, 0xe169c58d, 0xbc57ac4c, 0x9b00dbd8});
    STATIC_REQUIRE(philox4x32_10({0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff},
                                 {0xffffffff, 0xffffffff}) ==
                   block{0x408f276d, 0x41c83b0e, 0xa20bc7c6, 0x6d5451fd});
    STATIC_REQUIRE(philox4x32_10({0x243f6a88, 0x85a308d3, 0x13198a2e, 0x03707344},
                                 {0xa4093822, 0x299f31d0}) ==
                   block{0xd16cfe09, 0x94fdcceb, 0x5001e420, 0x24126ea1});
}

TEST_CASE("series_key is FNV-1a over the source name", "[rng_stream]") {
    // The published FNV-1a 64 values, so the key is the same on every platform.
    STATIC_REQUIRE(series_key("") == 0xcbf29ce484222325ull);
    STATIC_REQUIRE(series_key("a") == 0xaf63dc4c8601ec8cull);
    CHECK(series_key("acme.fx.EURUSD") != series_key("acme.fx.EURGBP"));
}

TEST_CASE("rng_stream gives one key the same draws whatever else is drawn", "[rng_stream]") {
    const stream_key eurusd{.run_seed = 42,
                            .series = series_key("acme.fx.EURUSD"),
                            .purpose = draw_purpose::arrival};
    const stream_key gbpusd{.run_seed = 42,
                            .series = series_key("acme.fx.GBPUSD"),
                            .purpose = draw_purpose::arrival};

    rng_stream alone(eurusd);
    const auto expected = take(alone, 9);

    // Another stream made first and drawn between every draw of this one, as a
    // second feed on the same run would, changes nothing.
    rng_stream other(gbpusd);
    other();
    rng_stream shared(eurusd);
    std::vector<std::uint64_t> actual;
    for (int i = 0; i < 9; ++i) {
        actual.push_back(shared());
        other();
        other();
    }
    CHECK(actual == expected);
    CHECK(shared.draws() == 9);
}

TEST_CASE("rng_stream draws differ when any part of the key differs", "[rng_stream]") {
    const auto series = series_key("acme.fx.EURUSD");
    const stream_key base{.run_seed = 7, .series = series, .purpose = draw_purpose::arrival};
    const std::array keys{
        base,
        stream_key{.run_seed = 8, .series = series, .purpose = draw_purpose::arrival},
        stream_key{.run_seed = 7, .series = series + 1, .purpose = draw_purpose::arrival},
        stream_key{.run_seed = 7, .series = series, .purpose = draw_purpose::price},
    };

    // Every value across the four streams is distinct: a stream that repeated a
    // block, or two keys that shared a range of the counter, would collide.
    std::set<std::uint64_t> seen;
    for (const auto& key : keys) {
        rng_stream stream(key);
        for (const auto value : take(stream, 1000))
            seen.insert(value);
    }
    CHECK(seen.size() == 4000);
}

TEST_CASE("rng_stream draws are uniform and two streams are uncorrelated", "[rng_stream]") {
    rng_stream a(stream_key{.run_seed = 1, .series = series_key("a"), .purpose = draw_purpose::price});
    rng_stream b(stream_key{.run_seed = 1, .series = series_key("b"), .purpose = draw_purpose::price});

    // With n draws the sample mean of a uniform has a standard error of
    // 1/sqrt(12 n), and the sample correlation of two independent streams one of
    // 1/sqrt(n); each bound is about five standard errors.
    constexpr int n = 100000;
    double sum_a = 0, sum_b = 0, sum_ab = 0, sum_aa = 0, sum_bb = 0;
    for (int i = 0; i < n; ++i) {
        const double x = uniform(a());
        const double y = uniform(b());
        sum_a += x;
        sum_b += y;
        sum_ab += x * y;
        sum_aa += x * x;
        sum_bb += y * y;
    }
    const double mean_a = sum_a / n;
    const double mean_b = sum_b / n;
    const double cov = sum_ab / n - mean_a * mean_b;
    const double corr =
        cov / std::sqrt((sum_aa / n - mean_a * mean_a) * (sum_bb / n - mean_b * mean_b));

    CHECK(mean_a == Catch::Approx(0.5).margin(0.005));
    CHECK(mean_b == Catch::Approx(0.5).margin(0.005));
    CHECK(std::abs(corr) < 0.016);
}

TEST_CASE("rng_stream drives the standard distributions", "[rng_stream]") {
    rng_stream stream(
        stream_key{.run_seed = 3, .series = series_key("gaps"), .purpose = draw_purpose::arrival});
    std::exponential_distribution<double> gap(4.0);

    constexpr int n = 100000;
    double sum = 0;
    for (int i = 0; i < n; ++i)
        sum += gap(stream);

    // An exponential of rate four has a mean of a quarter and a standard
    // deviation of a quarter, so the sample mean is within five standard errors.
    CHECK(sum / n == Catch::Approx(0.25).margin(5 * 0.25 / std::sqrt(n)));
}
