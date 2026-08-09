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
#ifndef ORES_ANALYTICS_QUANT_MATH_PSD_MATRIX_HPP
#define ORES_ANALYTICS_QUANT_MATH_PSD_MATRIX_HPP

#include "ores.analytics.quant/export.hpp"
#include <Eigen/Dense>
#include <string>

namespace ores::analytics::quant::math {

/**
 * @brief Square root of a symmetric positive-semidefinite matrix.
 *
 * Tries the Cholesky decomposition first, the efficient path for a
 * strictly positive-definite matrix; when it fails, the symmetric
 * eigen decomposition decides between the singular but
 * positive-semidefinite limit -- a legitimate degenerate input, e.g.
 * the zero covariance of a deterministic simulation -- and an
 * indefinite matrix, which is rejected. The rejection tolerance is
 * relative to the largest eigenvalue, so a tiny negative eigenvalue
 * from rounding (around the -1e-16 scale) does not reject a
 * well-formed matrix.
 *
 * @param matrix The symmetric matrix to root; a NaN entry fails the
 *               eigen decomposition's checks and is rejected.
 * @param error_message_prefix The process or caller that rejected the
 *                             matrix, prefixed to the exception
 *                             message.
 * @return A root R with R * R^T == matrix: the lower-triangular
 *         Cholesky factor when the matrix is positive definite, else
 *         the symmetric eigen root V * sqrt(D) * V^T.
 * @throws std::invalid_argument when the matrix is indefinite or the
 *         eigen decomposition fails.
 */
ORES_ANALYTICS_QUANT_EXPORT Eigen::MatrixXd
psd_matrix_square_root(const Eigen::MatrixXd& matrix, const std::string& error_message_prefix);

} // namespace ores::analytics::quant::math

#endif
