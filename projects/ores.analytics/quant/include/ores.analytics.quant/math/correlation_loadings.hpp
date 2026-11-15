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
#ifndef ORES_ANALYTICS_QUANT_MATH_CORRELATION_LOADINGS_HPP
#define ORES_ANALYTICS_QUANT_MATH_CORRELATION_LOADINGS_HPP

#include "ores.analytics.quant/export.hpp"
#include <Eigen/Core>
#include <string>

namespace ores::analytics::quant::math {

/**
 * @brief The loadings that realise a correlation matrix, as its Cholesky factor.
 *
 * Rows of the returned L are the loadings: row i has unit norm and
 * L * L^T == correlation, so a block's members correlate by the matrix a person
 * stated. The first row is the anchor's, exactly one on the first factor,
 * because the anchor's own driver is the block's first factor; a symmetric root
 * would not give that, which is why the Cholesky is required here rather than
 * psd_matrix_square_root.
 *
 * A set of numbers that is not a correlation matrix is refused, never
 * projected: a projected value is not the one a person stated.
 *
 * @param correlation The symmetric matrix, with a unit diagonal.
 * @param error_message_prefix The caller that rejected it, prefixed to the
 *        exception message.
 * @throws std::invalid_argument when the matrix is not square, not symmetric, a
 *         diagonal entry is not one, or the matrix is not positive definite.
 */
ORES_ANALYTICS_QUANT_EXPORT Eigen::MatrixXd
correlation_loadings(const Eigen::MatrixXd& correlation, const std::string& error_message_prefix);

}

#endif
