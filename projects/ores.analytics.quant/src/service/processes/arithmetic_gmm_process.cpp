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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.analytics.quant/service/processes/arithmetic_gmm_process.hpp"
#include <stdexcept>

namespace ores::analytics::quant::service {

arithmetic_gmm_process::arithmetic_gmm_process(std::vector<double> means,
                                               std::vector<double> stdevs,
                                               std::vector<double> weights,
                                               double initial_price,
                                               std::uint32_t seed)
    : means_(std::move(means))
    , stdevs_(std::move(stdevs))
    , weights_(std::move(weights))
    , price_(initial_price)
    , rng_(seed)
    , component_dist_(weights_.begin(), weights_.end()) {

    if (means_.size() != stdevs_.size() || means_.size() != weights_.size())
        throw std::invalid_argument(
            "arithmetic_gmm_process: means, stdevs, weights must have equal size");
    if (means_.empty())
        throw std::invalid_argument("arithmetic_gmm_process: at least one component required");
    if (initial_price <= 0.0)
        throw std::invalid_argument("arithmetic_gmm_process: initial_price must be positive");
}

double arithmetic_gmm_process::next() {
    const int k = component_dist_(rng_);
    // std::normal_distribution requires stddev > 0 (a libstdc++ assertion aborts
    // the process on σ <= 0 in debug builds). A zero-variance component is
    // degenerate: the draw is exactly the mean, so handle it directly.
    const double sd = stdevs_[k];
    const double increment =
        sd > 0.0 ? std::normal_distribution<double>(means_[k], sd)(rng_) : means_[k];
    price_ += increment;
    return price_;
}

double arithmetic_gmm_process::current() const {
    return price_;
}

}
