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
#include "ores.analytics.quant/service/processes/poisson_arrival_process.hpp"
#include <cmath>
#include <stdexcept>

namespace ores::analytics::quant::service {

poisson_arrival_process::poisson_arrival_process(double mean_rate, std::uint32_t seed)
    : mean_rate_(mean_rate)
    , gap_distribution_(mean_rate)
    , rng_(seed) {

    if (!std::isfinite(mean_rate_) || mean_rate_ <= 0.0)
        throw std::invalid_argument(
            "poisson_arrival_process: mean_rate must be finite and strictly positive");
}

double poisson_arrival_process::next_gap() {
    return gap_distribution_(rng_);
}

}
