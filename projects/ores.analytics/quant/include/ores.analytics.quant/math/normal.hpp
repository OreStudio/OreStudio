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

#include "ores.analytics.quant/export.hpp"

namespace ores::analytics::quant::math {

/// The standard normal density.
ORES_ANALYTICS_QUANT_EXPORT double normal_pdf(double x) noexcept;

/// The standard normal distribution function.
ORES_ANALYTICS_QUANT_EXPORT double normal_cdf(double x) noexcept;

/**
 * @brief The standard normal quantile: the x with normal_cdf(x) = p.
 *
 * Wichura's algorithm AS 241, PPND16 (Applied Statistics 37(3), 1988), which is
 * accurate to about one part in 10^16 over (0, 1). Minus infinity at zero, plus
 * infinity at one, and NaN outside [0, 1].
 */
ORES_ANALYTICS_QUANT_EXPORT double normal_quantile(double p) noexcept;

}

#endif
