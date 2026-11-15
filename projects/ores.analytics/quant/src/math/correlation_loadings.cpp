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
#include "ores.analytics.quant/math/correlation_loadings.hpp"
#include <Eigen/Cholesky>
#include <cmath>
#include <stdexcept>

namespace ores::analytics::quant::math {
namespace {

void require_correlation(const Eigen::MatrixXd& c, const std::string& prefix) {
    constexpr double tolerance = 1e-10;
    if (c.rows() == 0 || c.rows() != c.cols())
        throw std::invalid_argument(prefix + ": the matrix is not square");
    for (Eigen::Index i = 0; i < c.rows(); ++i) {
        if (!std::isfinite(c(i, i)) || std::abs(c(i, i) - 1.0) > tolerance)
            throw std::invalid_argument(prefix + ": the diagonal is not one");
        for (Eigen::Index j = i + 1; j < c.cols(); ++j) {
            if (!std::isfinite(c(i, j)) || std::abs(c(i, j) - c(j, i)) > tolerance)
                throw std::invalid_argument(prefix + ": the matrix is not symmetric");
            if (std::abs(c(i, j)) > 1.0 + tolerance)
                throw std::invalid_argument(prefix + ": an entry is outside [-1, 1]");
        }
    }
}

}

Eigen::MatrixXd correlation_loadings(const Eigen::MatrixXd& correlation,
                                     const std::string& error_message_prefix) {
    require_correlation(correlation, error_message_prefix);
    const Eigen::LLT<Eigen::MatrixXd> llt(correlation);
    if (llt.info() != Eigen::Success)
        throw std::invalid_argument(error_message_prefix + ": the matrix is not positive definite");
    return llt.matrixL();
}

}
