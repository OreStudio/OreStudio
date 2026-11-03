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
#ifndef ORES_ANALYTICS_QUANT_MATH_RETURN_MOMENTS_HPP
#define ORES_ANALYTICS_QUANT_MATH_RETURN_MOMENTS_HPP

#include "ores.analytics.quant/export.hpp"
#include <expected>
#include <span>
#include <string>

namespace ores::analytics::quant::math {

/**
 * @brief The first four moments of a sample of returns.
 *
 * The stdev is the sample one, over n - 1. The skewness m3 / m2^1.5 and the excess
 * kurtosis m4 / m2^2 - 3 use the central moments over n.
 */
struct return_moments final {
    int count;
    double mean;
    double stdev;
    double skewness;
    double excess_kurtosis;
};

/// The moments of a sample, refused below four returns or with no spread.
ORES_ANALYTICS_QUANT_EXPORT std::expected<return_moments, std::string>
moments_of(std::span<const double> returns);

/// The skewness and excess kurtosis of a return.
struct return_shape final {
    double skewness;
    double excess_kurtosis;
};

/**
 * @brief An authored belief about a shape, and how many returns it is worth.
 *
 * A sample of n returns moves the shape from the prior by n / (n + weight).
 */
struct shape_prior final {
    return_shape shape;
    double weight;
};

/**
 * @brief The sample's shape shrunk toward the prior.
 *
 * Each moment is w * sample + (1 - w) * prior, with w = n / (n + weight). The
 * standard errors of the two moments are about sqrt(6 / n) and sqrt(24 / n) for
 * normal data, so a year of returns alone leaves both noisy.
 */
ORES_ANALYTICS_QUANT_EXPORT return_shape shrink(const return_moments& sample,
                                                const shape_prior& prior) noexcept;

/**
 * @brief The shape of an n-day return of independent days with a daily shape.
 *
 * The skewness falls as one over sqrt(n) and the excess kurtosis as one over n.
 */
ORES_ANALYTICS_QUANT_EXPORT return_shape over_days(const return_shape& daily, double days) noexcept;

/**
 * @brief Whether the Gram-Charlier expansion with this shape is a density.
 *
 * The expansion 1 + skewness He3(z) / 6 + excess_kurtosis He4(z) / 24 must not be
 * negative for any z. Jondeau and Rockinger (2001, Journal of Economic Dynamics and
 * Control 25(10)) map the region: the excess kurtosis lies in [0, 4], and the
 * skewness in an interval about zero that is widest near 2.4 and closes at both ends.
 */
ORES_ANALYTICS_QUANT_EXPORT bool is_gram_charlier_density(const return_shape& shape) noexcept;

/**
 * @brief The shape held inside the Gram-Charlier region.
 *
 * The excess kurtosis is held to [0, 4], then the skewness is scaled toward zero
 * as little as keeps the expansion a density. A shape inside is returned unchanged.
 */
ORES_ANALYTICS_QUANT_EXPORT return_shape
hold_in_gram_charlier_region(const return_shape& shape) noexcept;

/**
 * @brief The annualised volatility of the last window returns: their sample stdev
 * times the square root of the periods in a year. Refused when the sample is
 * shorter than the window or the window shorter than two.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<double, std::string>
realised_volatility(std::span<const double> returns, int window, double periods_per_year);

}

#endif
