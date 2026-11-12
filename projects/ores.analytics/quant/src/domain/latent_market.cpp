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
#include "ores.analytics.quant/domain/latent_market.hpp"
#include <cmath>
#include <stdexcept>
#include <string>

namespace ores::analytics::quant::domain {

latent_market
make_latent_market(std::uint64_t run_seed, double cell_width, unsigned depth, double start) {
    latent_market market{};
    market.run_seed = run_seed;
    market.cell_width = cell_width;
    market.depth = depth;
    market.start = start;
    market.now = start;
    market.loading_begin.push_back(0);
    return market;
}

std::uint32_t latent_add_factor(latent_market& market, std::uint64_t factor_id) {
    const math::stream_key key{
        .run_seed = market.run_seed, .series = factor_id, .purpose = math::draw_purpose::market};
    const auto params = math::make_keyed_path(key, market.cell_width, market.depth, market.start);
    market.factors.push_back(params);
    market.walks.push_back(math::make_keyed_walk(params));
    market.factor_value.push_back(0.0);
    return static_cast<std::uint32_t>(market.factors.size() - 1);
}

std::uint32_t latent_add_series(latent_market& market) {
    market.driver.push_back(0.0);
    market.tick_time.push_back(market.start);
    market.loading_begin.push_back(market.loading_begin.back());
    return static_cast<std::uint32_t>(market.driver.size() - 1);
}

void latent_set_loadings(latent_market& market,
                         std::uint32_t series,
                         std::span<const factor_weight> loadings) {
    if (series + 1 != market.driver.size())
        throw std::invalid_argument(
            "latent_set_loadings: loadings are set for the last series added");

    double squares = 0.0;
    for (std::size_t i = 0; i < loadings.size(); ++i) {
        const factor_weight& weight = loadings[i];
        if (weight.factor >= market.factors.size())
            throw std::invalid_argument("latent_set_loadings: no factor " +
                                        std::to_string(weight.factor));
        if (!std::isfinite(weight.weight))
            throw std::invalid_argument("latent_set_loadings: a weight is not finite");
        for (std::size_t j = 0; j < i; ++j) {
            if (loadings[j].factor == weight.factor)
                throw std::invalid_argument("latent_set_loadings: a factor is repeated");
        }
        squares += weight.weight * weight.weight;
    }

    // The driver is a standard Brownian motion only while its shared weights and
    // its own motion account for one of variance. Anything above one is refused
    // rather than scaled, because a scaled loading is not the one a person set.
    constexpr double tolerance = 1e-12;
    if (squares > 1.0 + tolerance)
        throw std::invalid_argument("latent_set_loadings: the squared weights exceed one");

    market.loadings.insert(market.loadings.end(), loadings.begin(), loadings.end());
    market.loading_begin.back() = static_cast<std::uint32_t>(market.loadings.size());
}

void latent_advance(latent_market& market, double t) {
    if (!std::isfinite(t))
        throw std::invalid_argument("latent_advance: time must be finite");
    if (t < market.now)
        throw std::invalid_argument("latent_advance: time is behind the market");

    for (std::size_t k = 0; k < market.factors.size(); ++k) {
        market.factor_value[k] = math::keyed_walk_value(market.factors[k], market.walks[k], t);
    }
    market.now = t;
}

double latent_driver_value(const latent_market& market, std::uint32_t series) {
    double value = 0.0;
    const std::uint32_t begin = market.loading_begin[series];
    const std::uint32_t end = market.loading_begin[series + 1];
    for (std::uint32_t i = begin; i < end; ++i)
        value += market.loadings[i].weight * market.factor_value[market.loadings[i].factor];
    return value;
}

driver_increment latent_tick(latent_market& market, std::uint32_t series, double t) {
    latent_advance(market, t);

    const double value = latent_driver_value(market, series);
    driver_increment increment{};
    increment.shared = value - market.driver[series];
    increment.elapsed = market.now - market.tick_time[series];

    market.driver[series] = value;
    market.tick_time[series] = market.now;
    return increment;
}

}
