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
#include "ores.analytics.quant/service/processes/hawkes_arrival_process.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>

namespace ores::analytics::quant::service {

hawkes_arrival_process::hawkes_arrival_process(double mean_rate,
                                               double branching_ratio,
                                               double decay,
                                               std::uint32_t seed)
    : mean_rate_(mean_rate)
    , branching_ratio_(branching_ratio)
    , decay_(decay)
    , baseline_(mean_rate * (1.0 - branching_ratio))
    , jump_(branching_ratio * decay)
    , uniform_(std::nextafter(0.0, 1.0), 1.0)
    , rng_(seed) {

    if (!std::isfinite(mean_rate_) || mean_rate_ <= 0.0)
        throw std::invalid_argument(
            "hawkes_arrival_process: mean_rate must be finite and strictly positive");
    if (!std::isfinite(branching_ratio_) || branching_ratio_ < 0.0 || branching_ratio_ >= 1.0)
        throw std::invalid_argument(
            "hawkes_arrival_process: branching_ratio must be at least zero and strictly below one");
    if (!std::isfinite(decay_) || decay_ <= 0.0)
        throw std::invalid_argument(
            "hawkes_arrival_process: decay must be finite and strictly positive");
}

double hawkes_arrival_process::next_gap() {
    // The next event is the earlier of a baseline arrival and one from the
    // fading excess (Dassios and Zhao). uniform_ is drawn on (0, 1], so the
    // logarithms stay finite.
    const double baseline_gap = -std::log(uniform_(rng_)) / baseline_;

    auto excess_gap = std::numeric_limits<double>::infinity();
    if (excess_ > 0.0) {
        const double d = 1.0 + decay_ * std::log(uniform_(rng_)) / excess_;
        if (d > 0.0)
            excess_gap = -std::log(d) / decay_;
    }

    const double gap = std::min(baseline_gap, excess_gap);
    excess_ = excess_ * std::exp(-decay_ * gap) + jump_;
    return gap;
}

}
