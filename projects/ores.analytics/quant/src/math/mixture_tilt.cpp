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
#include "ores.analytics.quant/math/mixture_tilt.hpp"
#include <algorithm>
#include <cmath>
#include <cstddef>

namespace ores::analytics::quant::math {

mixture tilt(const mixture& base, double tilt) {
    if (tilt == 0.0 || base.stdevs.empty() ||
        std::ranges::any_of(base.stdevs, [](double s) { return !(s > 0.0); }))
        return base;

    const double power = std::max(0.0, 1.0 + tilt);
    // Relative to the widest, so the power stays in range for stdevs far below one.
    const double widest = std::ranges::max(base.stdevs);
    mixture tilted = base;
    double before = 0.0;
    double after = 0.0;
    for (std::size_t k = 0; k < base.stdevs.size(); ++k) {
        tilted.stdevs[k] = std::pow(base.stdevs[k] / widest, power);
        before += base.weights[k] * base.stdevs[k] * base.stdevs[k];
        after += base.weights[k] * tilted.stdevs[k] * tilted.stdevs[k];
    }
    const double scale = std::sqrt(before / after);
    for (auto& s : tilted.stdevs)
        s *= scale;
    return tilted;
}

}
