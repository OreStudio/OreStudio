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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.analytics.quant/math/psd_matrix.hpp"
#include <cmath>
#include <stdexcept>

namespace ores::analytics::quant::math {

Eigen::MatrixXd psd_matrix_square_root(const Eigen::MatrixXd& matrix,
                                       const std::string& error_message_prefix) {
    // LLT requires strict positive definiteness; a singular but
    // positive-semidefinite matrix (sigma == 0, or a rank-deficient
    // correlation) is a legitimate degenerate input -- the factors then
    // move deterministically -- so on an LLT failure the eigen
    // decomposition distinguishes it from an indefinite matrix, which is
    // rejected. The tolerance is relative to the largest eigenvalue: a
    // tiny negative eigenvalue from rounding alone (around the -1e-16
    // scale) is not an indefinite matrix.
    Eigen::LLT<Eigen::MatrixXd> llt(matrix);
    if (llt.info() == Eigen::Success)
        return llt.matrixL();

    Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> solver(matrix);
    if (solver.info() != Eigen::Success)
        throw std::invalid_argument(error_message_prefix);
    const double largest = solver.eigenvalues().maxCoeff();
    const double tolerance = -1e-10 * std::max(1.0, std::abs(largest));
    if (!(solver.eigenvalues().minCoeff() >= tolerance))
        throw std::invalid_argument(error_message_prefix);

    // A symmetric root: V * sqrt(D) * V' == matrix. It is not the
    // lower-triangular Cholesky factor, but any root realises the same
    // correlated-shock distribution.
    return solver.eigenvectors() * solver.eigenvalues().cwiseMax(0.0).cwiseSqrt().asDiagonal();
}

} // namespace ores::analytics::quant::math
