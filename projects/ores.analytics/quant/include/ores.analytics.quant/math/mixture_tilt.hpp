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
#ifndef ORES_ANALYTICS_QUANT_MATH_MIXTURE_TILT_HPP
#define ORES_ANALYTICS_QUANT_MATH_MIXTURE_TILT_HPP

#include "ores.analytics.quant/export.hpp"
#include <vector>

namespace ores::analytics::quant::math {

/// A Gaussian mixture: one mean, one standard deviation and one weight a component.
struct mixture final {
    std::vector<double> means;
    std::vector<double> stdevs;
    std::vector<double> weights;
};

/**
 * @brief Lean a mixture toward its tails, keeping its weights, its mean and its variance.
 *
 * Each stdev becomes stdev ^ (1 + tilt), and then all of them are scaled by one factor
 * so the mixture's variance is what it was. A positive tilt spreads the stdevs apart,
 * so the wide components get wider and the narrow ones narrower, and the kurtosis
 * rises. A negative tilt draws them together, down to one shared stdev at a tilt of
 * minus one, which is as far as it goes. The weights and the means do not move, so
 * neither does the mean.
 *
 * Moving weight toward the wide components would not do: at a fixed variance a scale
 * mixture's kurtosis peaks at a small wide weight, so moving weight past that peak
 * thins the tails. Weights of 0.8 and 0.2 on stdevs one to four fall from a kurtosis
 * of 9.75 to 4.4 when the weight moves. Spreading the stdevs raises the kurtosis for
 * any mixture whose stdevs differ.
 *
 * A zero tilt, or a mixture with a component of zero stdev, comes back unchanged.
 */
ORES_ANALYTICS_QUANT_EXPORT mixture tilt(const mixture& base, double tilt);

}

#endif
