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
#ifndef ORES_ANALYTICS_QUANT_MATH_FX_VOL_SURFACE_CORRELATION_HPP
#define ORES_ANALYTICS_QUANT_MATH_FX_VOL_SURFACE_CORRELATION_HPP

#include "ores.analytics.quant/export.hpp"
#include <Eigen/Core>
#include <cstddef>
#include <expected>
#include <string>

namespace ores::analytics::quant::math {

/**
 * @brief The correlation of a volatility surface's shocks, built from the three numbers a feed
 * states.
 *
 * A feed states three numbers, because the rest are independent: the correlation of spot with
 * every forward variance, the correlation of spot with the skew driver, and the correlation of
 * neighbouring forward variances, which falls by that factor once for each segment between them.
 * The curvature driver is independent of every other shock.
 *
 * The order is the one the generator and the latent market both use: spot first, then each
 * segment's forward variance in expiry order, then the skew driver, then the curvature driver.
 *
 * The matrix is refused when it is not positive definite, never projected onto the nearest one:
 * three numbers a person stated that no set of standard normals can realise are a configuration
 * fault, not something to approximate.
 *
 * @param segments How many forward variances the surface has.
 * @param spot_variance The correlation of spot with every forward variance.
 * @param spot_skew The correlation of spot with the skew driver.
 * @param neighbour The correlation of neighbouring forward variances.
 * @return The matrix, or why the three numbers do not describe a correlation.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::expected<Eigen::MatrixXd, std::string>
fx_vol_surface_correlation(std::size_t segments,
                           double spot_variance,
                           double spot_skew,
                           double neighbour);

}

#endif
