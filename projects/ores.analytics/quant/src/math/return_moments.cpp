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
#include "ores.analytics.quant/math/return_moments.hpp"
#include "ores.analytics.quant/math/brent.hpp"
#include <algorithm>
#include <cmath>
#include <cstddef>
#include <initializer_list>
#include <utility>

namespace ores::analytics::quant::math {

namespace {

/// How far below zero rounding may leave a minimum that is zero, as at the region's edge.
constexpr double density_tolerance = 1e-12;

/// The steps of the bisection that scales a skewness back into the region.
constexpr int scaling_steps = 60;

double expansion_at(const return_shape& shape, double z) noexcept {
    const double z2 = z * z;
    const double he3 = z * (z2 - 3.0);
    const double he4 = z2 * z2 - 6.0 * z2 + 3.0;
    return 1.0 + shape.skewness * he3 / 6.0 + shape.excess_kurtosis * he4 / 24.0;
}

/// The expansion's slope: skewness (z^2 - 1) / 2 + excess_kurtosis (z^3 - 3 z) / 6.
double slope_at(const return_shape& shape, double z) noexcept {
    return shape.skewness * (z * z - 1.0) / 2.0 + shape.excess_kurtosis * z * (z * z - 3.0) / 6.0;
}

}

std::expected<return_moments, std::string> moments_of(std::span<const double> returns) {
    if (returns.size() < 4)
        return std::unexpected("the moments of a sample need at least four returns");
    const auto n = static_cast<double>(returns.size());
    double mean = 0.0;
    for (const double r : returns)
        mean += r;
    mean /= n;
    double m2 = 0.0;
    double m3 = 0.0;
    double m4 = 0.0;
    for (const double r : returns) {
        const double d = r - mean;
        const double d2 = d * d;
        m2 += d2;
        m3 += d2 * d;
        m4 += d2 * d2;
    }
    m2 /= n;
    m3 /= n;
    m4 /= n;
    // A constant series leaves only rounding residue once demeaned, of the order of
    // epsilon squared times its mean squared, so a spread below a part in 10^24 of the
    // mean's square, or below 10^-30 absolutely, is no spread.
    if (!std::isfinite(m2) || !(m2 > 1e-30 + 1e-24 * mean * mean))
        return std::unexpected("the returns have no spread");
    return return_moments{.count = static_cast<int>(returns.size()),
                          .mean = mean,
                          .stdev = std::sqrt(m2 * n / (n - 1.0)),
                          .skewness = m3 / (m2 * std::sqrt(m2)),
                          .excess_kurtosis = m4 / (m2 * m2) - 3.0};
}

return_shape shrink(const return_moments& sample, const shape_prior& prior) noexcept {
    const double n = sample.count;
    const double w = prior.weight > 0.0 ? n / (n + prior.weight) : 1.0;
    return {.skewness = w * sample.skewness + (1.0 - w) * prior.shape.skewness,
            .excess_kurtosis =
                w * sample.excess_kurtosis + (1.0 - w) * prior.shape.excess_kurtosis};
}

return_shape over_days(const return_shape& daily, double days) noexcept {
    return {.skewness = daily.skewness / std::sqrt(days),
            .excess_kurtosis = daily.excess_kurtosis / days};
}

bool is_gram_charlier_density(const return_shape& shape) noexcept {
    if (!std::isfinite(shape.skewness) || !std::isfinite(shape.excess_kurtosis))
        return false;
    if (shape.excess_kurtosis < 0.0 || shape.excess_kurtosis > 4.0)
        return false;
    const double s = shape.skewness;
    const double k = shape.excess_kurtosis;
    // With no kurtosis the expansion is a cubic, negative somewhere unless it has no skew.
    if (k == 0.0)
        return s == 0.0;
    // The expansion rises without bound both ways, so it is lowest at a root of its slope
    // k z^3 / 6 + s z^2 / 2 - k z / 2 - s / 2. The slope turns where its own slope
    // k z^2 / 2 + s z - k / 2 is zero, which splits the line into three pieces on each of
    // which the slope is monotone and has at most one root, and Cauchy's bound
    // 1 + max(3, 3 |s| / k) holds every root, so it closes the two outer pieces.
    const double root_of_discriminant = std::sqrt(s * s + k * k);
    const double turn_low = (-s - root_of_discriminant) / k;
    const double turn_high = (-s + root_of_discriminant) / k;
    const double bound = 1.0 + std::max(3.0, 3.0 * std::abs(s) / k);
    const auto slope = [&](double z) {
        return slope_at(shape, z);
    };
    for (const auto& [lo, hi] : {std::pair{-bound, turn_low},
                                 std::pair{turn_low, turn_high},
                                 std::pair{turn_high, bound}}) {
        const auto root = brent_root(slope, lo, hi, {.tolerance = 1e-14});
        if (root && expansion_at(shape, *root) < -density_tolerance)
            return false;
    }
    return true;
}

return_shape hold_in_gram_charlier_region(const return_shape& shape) noexcept {
    if (is_gram_charlier_density(shape))
        return shape;
    const double kurtosis =
        std::isfinite(shape.excess_kurtosis) ? std::clamp(shape.excess_kurtosis, 0.0, 4.0) : 0.0;
    const double skewness = std::isfinite(shape.skewness) ? shape.skewness : 0.0;
    double inside = 0.0;
    double outside = 1.0;
    if (is_gram_charlier_density({.skewness = skewness, .excess_kurtosis = kurtosis}))
        return {.skewness = skewness, .excess_kurtosis = kurtosis};
    for (int i = 0; i < scaling_steps; ++i) {
        const double mid = 0.5 * (inside + outside);
        if (is_gram_charlier_density({.skewness = mid * skewness, .excess_kurtosis = kurtosis}))
            inside = mid;
        else
            outside = mid;
    }
    return {.skewness = inside * skewness, .excess_kurtosis = kurtosis};
}

std::expected<double, std::string>
realised_volatility(std::span<const double> returns, int window, double periods_per_year) {
    if (window < 2)
        return std::unexpected("a realised volatility needs a window of at least two returns");
    if (returns.size() < static_cast<std::size_t>(window))
        return std::unexpected("the sample is shorter than the window");
    const auto last = returns.last(static_cast<std::size_t>(window));
    double mean = 0.0;
    for (const double r : last)
        mean += r;
    mean /= window;
    double ss = 0.0;
    for (const double r : last)
        ss += (r - mean) * (r - mean);
    return std::sqrt(ss / (window - 1.0) * periods_per_year);
}

}
