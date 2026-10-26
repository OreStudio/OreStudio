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
// Inspired by the tick library (BSD 3-clause, the tick developers, CMAP); see
// doc/knowledge/external/tick.org.
#include "ores.analytics.quant/service/processes/periodic_arrival_process.hpp"
#include <cmath>
#include <stdexcept>

namespace ores::analytics::quant::service {

namespace {

// The reciprocal needs a positive rate, so the check runs before the member
// that takes it is built.
double checked_rate(double mean_rate) {
    if (!std::isfinite(mean_rate) || mean_rate <= 0.0)
        throw std::invalid_argument(
            "periodic_arrival_process: mean_rate must be finite and strictly positive");
    return mean_rate;
}

}

periodic_arrival_process::periodic_arrival_process(double mean_rate, std::uint32_t seed)
    : mean_rate_(checked_rate(mean_rate))
    , gap_(1.0 / mean_rate_) {
    // A quarter turn would still leave four feeds in step; the phase is drawn, so
    // two feeds of one rate are as likely to be a whole gap apart as a hair.
    std::mt19937 rng(seed);
    std::uniform_real_distribution<double> uniform(0.0, 1.0);
    phase_ = uniform(rng);
}

double periodic_arrival_process::next_gap() {
    if (first_) {
        first_ = false;
        return gap_ * phase_;
    }
    return gap_;
}

void periodic_arrival_process::set_baseline_multiplier(double multiplier) {
    if (!std::isfinite(multiplier) || multiplier <= 0.0)
        throw std::invalid_argument("periodic_arrival_process: multiplier must be finite and "
                                    "strictly positive");
    multiplier_ = multiplier;
    gap_ = 1.0 / (mean_rate_ * multiplier_);
}

}
