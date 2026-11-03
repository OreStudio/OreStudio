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
#ifndef ORES_ANALYTICS_QUANT_MATH_NORMAL_HPP
#define ORES_ANALYTICS_QUANT_MATH_NORMAL_HPP

#include <boost/math/special_functions/erf.hpp>
#include <cmath>
#include <numbers>

namespace ores::analytics::quant::math {

/// The standard normal density.
inline double normal_pdf(double x) noexcept {
    return std::exp(-0.5 * x * x) * (std::numbers::inv_sqrtpi / std::numbers::sqrt2);
}

/// The standard normal distribution function.
inline double normal_cdf(double x) noexcept {
    return 0.5 * std::erfc(-x / std::numbers::sqrt2);
}

/**
 * @brief The standard normal quantile, for p in (0, 1).
 *
 * The C++ library has no inverse of erfc, so Boost.Math's is used:
 * x = -sqrt(2) erfc^-1(2 p). Boost raises an error at zero, one or outside.
 */
inline double normal_quantile(double p) {
    return -std::numbers::sqrt2 * boost::math::erfc_inv(2.0 * p);
}

}

#endif
