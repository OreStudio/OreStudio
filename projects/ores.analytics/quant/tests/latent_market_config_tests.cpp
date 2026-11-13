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
#include "ores.analytics.quant/domain/latent_market_config.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstdint>
#include <numeric>
#include <stdexcept>
#include <vector>

using ores::analytics::quant::domain::block_spec;
using ores::analytics::quant::domain::depth_for;
using ores::analytics::quant::domain::latent_config;
using ores::analytics::quant::domain::latent_tick;
using ores::analytics::quant::domain::make_latent_market;

namespace {

constexpr double config_hour = 3600.0;

latent_config one_block(double rho, std::vector<double> matrix = {}) {
    latent_config config{};
    config.run_seed = 1234;
    config.cell_width = config_hour;
    config.start = 0.0;
    config.fastest_gap = 60.0;
    config.blocks.push_back(block_spec{
        .name = "EURUSD",
        .anchor = 11,
        .members = {12},
        .size = 2,
        .correlation = matrix.empty() ? std::vector<double>{1.0, rho, rho, 1.0} : matrix});
    return config;
}

}

TEST_CASE("the depth follows the fastest gap a run can produce", "[latent_market_config]") {
    // The design note's own table: an hour of cells, a gap of about 55
    // milliseconds, a depth of 16.
    CHECK(depth_for(config_hour, 0.055) == 16);
    CHECK(depth_for(config_hour, 1.0) == 12);
    // A gap no shorter than a cell needs no bridge at all.
    CHECK(depth_for(config_hour, config_hour) == 1);
    CHECK(depth_for(config_hour, 2.0 * config_hour) == 1);

    // The finest step never exceeds the gap it was chosen for.
    for (const double gap : {10.0, 1.0, 0.25, 0.001}) {
        const unsigned depth = depth_for(config_hour, gap);
        CHECK(config_hour / std::ldexp(1.0, static_cast<int>(depth)) <= gap);
    }

    // A gap too small to resolve at the bridge's bound is refused, not rounded.
    CHECK_THROWS_AS(depth_for(config_hour, 1e-9), std::invalid_argument);
    CHECK_THROWS_AS(depth_for(config_hour, 0.0), std::invalid_argument);
    CHECK_THROWS_AS(depth_for(0.0, 1.0), std::invalid_argument);
}

TEST_CASE("a block builds a market whose drivers correlate as stated", "[latent_market_config]") {
    constexpr double rho = 0.6;
    constexpr int runs = 300;
    constexpr int ticks = 40;
    const double dt = config_hour / static_cast<double>(ticks);

    std::vector<double> anchor;
    std::vector<double> member;
    for (int r = 0; r < runs; ++r) {
        latent_config config = one_block(rho);
        config.run_seed = static_cast<std::uint64_t>(2000 + r);
        auto market = make_latent_market(config);
        for (int i = 1; i <= ticks; ++i) {
            const double t = static_cast<double>(i) * dt;
            anchor.push_back(latent_tick(market, 0, t).shared);
            member.push_back(latent_tick(market, 1, t).shared);
        }
    }

    const double n = static_cast<double>(anchor.size());
    const double mean_a = std::accumulate(anchor.begin(), anchor.end(), 0.0) / n;
    const double mean_b = std::accumulate(member.begin(), member.end(), 0.0) / n;
    double cov = 0.0;
    double var_a = 0.0;
    double var_b = 0.0;
    for (std::size_t i = 0; i < anchor.size(); ++i) {
        cov += (anchor[i] - mean_a) * (member[i] - mean_b);
        var_a += (anchor[i] - mean_a) * (anchor[i] - mean_a);
        var_b += (member[i] - mean_b) * (member[i] - mean_b);
    }
    const double correlation = cov / std::sqrt(var_a * var_b);
    CHECK(std::abs(correlation - rho) < 0.03);
}

TEST_CASE("a block that is not a correlation is refused", "[latent_market_config]") {
    // A diagonal that is not one, and a symmetric matrix that is not positive
    // definite: both are refused rather than projected onto the nearest one.
    CHECK_THROWS_AS(make_latent_market(one_block(0.6, {1.0, 0.6, 0.6, 1.5})),
                    std::invalid_argument);

    latent_config config{};
    config.run_seed = 7;
    config.cell_width = config_hour;
    config.fastest_gap = 60.0;
    config.blocks.push_back(
        block_spec{.name = "broken",
                   .anchor = 1,
                   .members = {2, 3},
                   .size = 3,
                   .correlation = {1.0, 0.9, 0.9, 0.9, 1.0, -0.9, 0.9, -0.9, 1.0}});
    CHECK_THROWS_AS(make_latent_market(config), std::invalid_argument);
}

TEST_CASE("a block whose size disagrees with its series is refused", "[latent_market_config]") {
    latent_config config = one_block(0.6);
    config.blocks.front().size = 3;
    CHECK_THROWS_AS(make_latent_market(config), std::invalid_argument);
}
