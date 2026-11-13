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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_LATENT_MARKET_CONFIG_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_LATENT_MARKET_CONFIG_HPP

#include "ores.analytics.quant/domain/latent_market.hpp"
#include "ores.analytics.quant/export.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::analytics::quant::domain {

/// One factor as configuration names it.
struct factor_spec {
    /// The factor's stream id.
    std::uint64_t id;
    /// What a screen and a trace call it.
    std::string name;
};

/**
 * @brief A block of series that must correlate beyond the global factors.
 *
 * The anchor comes first in the correlation matrix, so the loadings that realise
 * it give the anchor row one on its own driver and zero on the block's others.
 * The matrix is stored row-major and as numbers a person stated: it is refused
 * when it is not a correlation, never projected.
 *
 * A block whose anchor another block already anchors on shares that anchor's
 * factor and series. A run has thousands of surfaces on a handful of spots, and
 * two independent copies of one spot would let two surfaces on one pair drift
 * apart.
 */
struct block_spec {
    /// What a screen and a trace call it.
    std::string name;
    /// The anchor series' stream id.
    std::uint64_t anchor;
    /// The other series' stream ids, in correlation-matrix order after the anchor.
    std::vector<std::uint64_t> members;
    /// The number of rows and columns of the correlation matrix.
    std::size_t size;
    /// The correlation matrix, row-major, anchor first.
    std::vector<double> correlation;
};

/// Everything a run needs to build its latent market.
struct latent_config {
    std::uint64_t run_seed = 0;
    /// The width of a factor's cells in simulated time.
    double cell_width = 0.0;
    /// The simulated time the run starts at.
    double start = 0.0;
    /// The shortest gap the run's arrival laws can produce, in simulated time.
    /// The depth follows from it, so a burst is never smoother than its gap.
    double fastest_gap = 0.0;
    /// The factors every block may load on, before the block's own.
    std::vector<factor_spec> factors;
    /// The blocks, each with its anchor, members and stated correlation.
    std::vector<block_spec> blocks;
};

/**
 * @brief The depth that resolves the fastest gap a run can produce.
 *
 * The finest step is cell_width / 2^depth. A depth is enough when that step is
 * no wider than the shortest gap, which is 2^depth >= cell_width / fastest_gap.
 * Choosing it this way is what keeps a Hawkes burst from being interpolated into
 * smoothness: the resolution follows the arrivals instead of a number typed in.
 *
 * @throws std::invalid_argument when a width or a gap is not positive, or when
 *         resolving the gap would need a depth beyond the bridge's bound.
 */
ORES_ANALYTICS_QUANT_EXPORT unsigned depth_for(double cell_width, double fastest_gap);

/**
 * @brief Builds a market from configuration, checking it.
 *
 * Each block adds one factor for its anchor's own driver and one for each column
 * of its correlation matrix after the first, then sets the anchor's and the
 * members' loadings from the matrix's Cholesky factor.
 *
 * @throws std::invalid_argument when the depth cannot resolve the fastest gap,
 *         when a block's matrix is not a correlation, or when its size disagrees
 *         with the series it names.
 */
ORES_ANALYTICS_QUANT_EXPORT latent_market make_latent_market(const latent_config& config);

}

#endif
