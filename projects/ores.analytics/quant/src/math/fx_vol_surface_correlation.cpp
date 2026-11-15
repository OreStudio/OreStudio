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
#include "ores.analytics.quant/math/fx_vol_surface_correlation.hpp"
#include <Eigen/Cholesky>
#include <cmath>
#include <format>
#include <utility>

namespace ores::analytics::quant::math {

std::expected<Eigen::MatrixXd, std::string> fx_vol_surface_correlation(std::size_t segments,
                                                                       double spot_variance,
                                                                       double spot_skew,
                                                                       double neighbour) {
    if (segments == 0)
        return std::unexpected(std::string("A surface has at least one forward variance."));
    const std::pair<const char*, double> stated[] = {
        {"spot with the forward variance", spot_variance},
        {"spot with the skew", spot_skew},
        {"neighbouring forward variances", neighbour}};
    for (const auto& [what, value] : stated)
        if (!(std::abs(value) < 1.0))
            return std::unexpected(std::format(
                "The correlation of {} is {}, which is not inside (-1, 1).", what, value));

    const auto size = static_cast<Eigen::Index>(segments + 3);
    Eigen::MatrixXd correlation = Eigen::MatrixXd::Identity(size, size);
    const auto skew = static_cast<Eigen::Index>(segments + 1);
    correlation(0, skew) = correlation(skew, 0) = spot_skew;
    for (std::size_t i = 0; i < segments; ++i) {
        const auto row = static_cast<Eigen::Index>(i + 1);
        correlation(0, row) = correlation(row, 0) = spot_variance;
        for (std::size_t j = 0; j < segments; ++j) {
            if (i == j)
                continue;
            const auto distance = static_cast<double>(i > j ? i - j : j - i);
            correlation(row, static_cast<Eigen::Index>(j + 1)) = std::pow(neighbour, distance);
        }
    }
    const Eigen::LLT<Eigen::MatrixXd> root(correlation);
    if (root.info() != Eigen::Success)
        return std::unexpected(std::format(
            "Spot at {}, the skew at {} and neighbouring segments at {} are not a correlation of "
            "{} segment(s): the matrix they describe is not positive definite.",
            spot_variance,
            spot_skew,
            neighbour,
            segments));
    return correlation;
}

}
