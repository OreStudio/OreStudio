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
#include "ores.analytics.quant/math/correlation_loadings.hpp"
#include <Eigen/Core>
#include <cmath>
#include <stdexcept>
#include <string>

namespace ores::analytics::quant::domain {

unsigned depth_for(double cell_width, double fastest_gap) {
    if (!std::isfinite(cell_width) || cell_width <= 0.0)
        throw std::invalid_argument("depth_for: cell_width must be finite and positive");
    if (!std::isfinite(fastest_gap) || fastest_gap <= 0.0)
        throw std::invalid_argument("depth_for: fastest_gap must be finite and positive");
    if (fastest_gap >= cell_width)
        return 1;

    const double ratio = cell_width / fastest_gap;
    unsigned depth = 1;
    while (depth < 32 && std::ldexp(1.0, depth) < ratio)
        ++depth;
    if (std::ldexp(1.0, depth) < ratio)
        throw std::invalid_argument(
            "depth_for: resolving the fastest gap needs a depth beyond 32; widen the cell or "
            "slow the arrivals");
    return depth;
}

latent_market make_latent_market(const latent_config& config) {
    const unsigned depth = depth_for(config.cell_width, config.fastest_gap);
    latent_market market =
        make_latent_market(config.run_seed, config.cell_width, depth, config.start);

    for (const factor_spec& factor : config.factors)
        latent_add_factor(market, factor.id);

    for (const block_spec& block : config.blocks) {
        const std::size_t count = 1 + block.members.size();
        if (block.size != count)
            throw std::invalid_argument("make_latent_market: block '" + block.name +
                                        "' names a size that does not match its series");
        if (block.correlation.size() != count * count)
            throw std::invalid_argument("make_latent_market: block '" + block.name +
                                        "' has a correlation matrix of the wrong size");

        Eigen::MatrixXd correlation(static_cast<Eigen::Index>(count),
                                    static_cast<Eigen::Index>(count));
        for (std::size_t i = 0; i < count; ++i)
            for (std::size_t j = 0; j < count; ++j)
                correlation(static_cast<Eigen::Index>(i), static_cast<Eigen::Index>(j)) =
                    block.correlation[i * count + j];

        const Eigen::MatrixXd loadings = math::correlation_loadings(
            correlation, "make_latent_market: block '" + block.name + "'");

        // The anchor's own driver is the block's first factor.
        const std::uint32_t anchor_factor = latent_add_factor(market, block.anchor);
        const std::uint64_t block_key = math::series_key(block.name);
        const std::uint32_t first_own = static_cast<std::uint32_t>(market.factors.size());
        for (std::size_t column = 1; column < count; ++column)
            static_cast<void>(latent_add_factor(market, block_key + column));

        std::vector<factor_weight> weights;
        weights.reserve(count);
        const std::uint32_t anchor_series = latent_add_series(market, block.anchor);
        weights.push_back({.factor = anchor_factor, .weight = loadings(0, 0)});
        latent_set_loadings(market, anchor_series, weights);

        for (std::size_t row = 1; row < count; ++row) {
            const std::uint32_t series = latent_add_series(market, block.members[row - 1]);
            weights.clear();
            if (loadings(static_cast<Eigen::Index>(row), 0) != 0.0)
                weights.push_back({.factor = anchor_factor,
                                   .weight = loadings(static_cast<Eigen::Index>(row), 0)});
            for (std::size_t column = 1; column < count; ++column) {
                const double weight =
                    loadings(static_cast<Eigen::Index>(row), static_cast<Eigen::Index>(column));
                if (weight != 0.0)
                    weights.push_back({.factor = first_own + static_cast<std::uint32_t>(column - 1),
                                       .weight = weight});
            }
            latent_set_loadings(market, series, weights);
        }
    }
    return market;
}

}
