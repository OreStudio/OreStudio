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
#include "ores.analytics.quant/domain/fx_vol_surface_block.hpp"
#include "ores.analytics.quant/domain/latent_market.hpp"
#include "ores.analytics.quant/math/fx_vol_surface_correlation.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstdint>
#include <stdexcept>
#include <string>
#include <vector>

using namespace ores::analytics::quant::domain;

namespace {

constexpr double surface_hour = 3600.0;
constexpr std::uint64_t surface_spot = 4711;
constexpr double surface_dt = 60.0;
const std::string surface_name = "synthetic.fx_vol.eurusd";

/// The index of a series in a market, found by the id the block gave it.
std::uint32_t surface_series(const latent_market& market, std::uint64_t id) {
    for (std::uint32_t s = 0; s < market.series_id.size(); ++s)
        if (market.series_id[s] == id)
            return s;
    throw std::runtime_error("the market holds no series with that id");
}

/// One series' weights on the market's factors, by factor index.
std::vector<double> surface_loadings(const latent_market& market, std::uint32_t series) {
    std::vector<double> out(market.factors.size(), 0.0);
    for (auto i = market.loading_begin[series]; i < market.loading_begin[series + 1]; ++i)
        out[market.loadings[i].factor] = market.loadings[i].weight;
    return out;
}

double surface_dot(const std::vector<double>& a, const std::vector<double>& b) {
    double sum = 0.0;
    for (std::size_t i = 0; i < a.size(); ++i)
        sum += a[i] * b[i];
    return sum;
}

double surface_corr(const std::vector<double>& x, const std::vector<double>& y) {
    const auto n = static_cast<double>(x.size());
    double mx = 0.0;
    double my = 0.0;
    for (std::size_t i = 0; i < x.size(); ++i) {
        mx += x[i] / n;
        my += y[i] / n;
    }
    double sxy = 0.0;
    double sxx = 0.0;
    double syy = 0.0;
    for (std::size_t i = 0; i < x.size(); ++i) {
        sxy += (x[i] - mx) * (y[i] - my);
        sxx += (x[i] - mx) * (x[i] - mx);
        syy += (y[i] - my) * (y[i] - my);
    }
    return sxy / std::sqrt(sxx * syy);
}

latent_config surface_config(const block_spec& block) {
    latent_config config{};
    config.run_seed = 99;
    config.cell_width = surface_hour;
    config.start = 0.0;
    config.fastest_gap = surface_dt;
    config.blocks.push_back(block);
    return config;
}

}

TEST_CASE("a surface's block anchors on its spot and names its drivers in order",
          "[fx_vol_surface_block]") {
    const auto block = fx_vol_surface_block(surface_name, surface_spot, 5, 0.3, -0.4, 0.9);
    REQUIRE(block);
    CHECK(block->name == surface_name);
    CHECK(block->anchor == surface_spot);
    CHECK(block->size == 8);
    REQUIRE(block->members.size() == 7);
    // One forward variance for each segment, then the skew, then the curvature.
    for (std::size_t i = 0; i < 7; ++i)
        CHECK(block->members[i] == fx_vol_surface_driver_id(surface_name, i));
    CHECK(block->members[0] != block->anchor);
    // The matrix the block carries is the one the generator would have used.
    const auto matrix = ores::analytics::quant::math::fx_vol_surface_correlation(5, 0.3, -0.4, 0.9);
    REQUIRE(matrix);
    for (std::size_t i = 0; i < block->size; ++i)
        for (std::size_t j = 0; j < block->size; ++j)
            CHECK(
                block->correlation[i * block->size + j] ==
                Catch::Approx((*matrix)(static_cast<Eigen::Index>(i), static_cast<Eigen::Index>(j)))
                    .margin(1e-15));
}

TEST_CASE("a surface's loadings give every stated correlation exactly", "[fx_vol_surface_block]") {
    const auto block = fx_vol_surface_block(surface_name, surface_spot, 5, 0.3, -0.4, 0.9);
    REQUIRE(block);
    auto market = make_latent_market(surface_config(*block));

    const auto spot = surface_series(market, surface_spot);
    const auto first = surface_series(market, fx_vol_surface_driver_id(surface_name, 0));
    const auto last = surface_series(market, fx_vol_surface_driver_id(surface_name, 4));
    const auto skew = surface_series(market, fx_vol_surface_driver_id(surface_name, 5));
    const auto curvature = surface_series(market, fx_vol_surface_driver_id(surface_name, 6));

    const auto b_spot = surface_loadings(market, spot);
    // The anchor is its own factor entirely, with nothing left to its own motion.
    CHECK(surface_dot(b_spot, b_spot) == Catch::Approx(1.0).margin(1e-14));

    // Two drivers correlate by the dot product of their loadings, exactly.
    CHECK(surface_dot(b_spot, surface_loadings(market, first)) == Catch::Approx(0.3).margin(1e-14));
    CHECK(surface_dot(b_spot, surface_loadings(market, last)) == Catch::Approx(0.3).margin(1e-14));
    CHECK(surface_dot(b_spot, surface_loadings(market, skew)) == Catch::Approx(-0.4).margin(1e-14));
    CHECK(surface_dot(b_spot, surface_loadings(market, curvature)) == 0.0);
    // Neighbouring segments move together at 0.9, and the correlation falls by that factor
    // once for each segment between them: 0.81 one apart, 0.9 to the fourth for four apart.
    const auto second = surface_series(market, fx_vol_surface_driver_id(surface_name, 1));
    CHECK(surface_dot(surface_loadings(market, first), surface_loadings(market, second)) ==
          Catch::Approx(0.9).margin(1e-14));
    CHECK(surface_dot(surface_loadings(market, first), surface_loadings(market, last)) ==
          Catch::Approx(std::pow(0.9, 4.0)).margin(1e-14));
}

TEST_CASE("a surface's driver moves with its spot as the feed states", "[fx_vol_surface_block]") {
    // The loadings are exact, so this is the ordinary normal the feed hands its generator,
    // recovered by sampling the drivers at the same instants.
    constexpr double spot_variance = 0.3;
    constexpr double spot_skew = -0.4;
    constexpr int runs = 400;
    constexpr int ticks = 25;
    const auto block =
        fx_vol_surface_block(surface_name, surface_spot, 5, spot_variance, spot_skew, 0.9);
    REQUIRE(block);

    std::vector<double> spot;
    std::vector<double> segment;
    std::vector<double> skew;
    std::vector<double> curvature;
    for (int r = 0; r < runs; ++r) {
        auto config = surface_config(*block);
        config.run_seed = 1000 + static_cast<std::uint64_t>(r);
        auto market = make_latent_market(config);
        const auto s_spot = surface_series(market, surface_spot);
        const auto s_segment = surface_series(market, fx_vol_surface_driver_id(surface_name, 0));
        const auto s_skew = surface_series(market, fx_vol_surface_driver_id(surface_name, 5));
        const auto s_curvature = surface_series(market, fx_vol_surface_driver_id(surface_name, 6));
        for (int i = 1; i <= ticks; ++i) {
            const double t = static_cast<double>(i) * surface_dt;
            spot.push_back(latent_driver(market, s_spot, t));
            segment.push_back(latent_driver(market, s_segment, t));
            skew.push_back(latent_driver(market, s_skew, t));
            curvature.push_back(latent_driver(market, s_curvature, t));
        }
    }

    CHECK(surface_corr(spot, segment) == Catch::Approx(spot_variance).margin(0.04));
    CHECK(surface_corr(spot, skew) == Catch::Approx(spot_skew).margin(0.04));
    CHECK(std::abs(surface_corr(spot, curvature)) < 0.04);
}

TEST_CASE("a surface's block refuses a name it cannot key its drivers by",
          "[fx_vol_surface_block]") {
    CHECK_FALSE(fx_vol_surface_block("", surface_spot, 5, 0.3, -0.4, 0.9).has_value());
    // The three numbers are refused before any driver is named.
    CHECK_FALSE(fx_vol_surface_block(surface_name, surface_spot, 5, 1.0, -0.4, 0.9).has_value());
    CHECK_FALSE(fx_vol_surface_block(surface_name, surface_spot, 0, 0.3, -0.4, 0.9).has_value());
}
