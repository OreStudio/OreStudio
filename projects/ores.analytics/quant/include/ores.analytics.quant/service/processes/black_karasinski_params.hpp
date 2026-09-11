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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_BLACK_KARASINSKI_PARAMS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_BLACK_KARASINSKI_PARAMS_HPP

namespace ores::analytics::quant::service {

/**
 * @brief Strongly-typed parameters of the black_karasinski_process.
 *
 * The row-based parameter architecture stores process parameters as
 * {name, value} pairs (see ores.synthetic's parameter definition and
 * value entities); this struct is the strongly-typed "view" of those
 * rows that the mapping layer materialises before constructing the
 * process. It is deliberately a plain aggregate -- no methods, no
 * invariants (the process constructor and the mapping layer validate).
 *
 * The Black-Karasinski model is written on the *logarithm* of the short
 * rate: ln r follows an Ornstein-Uhlenbeck process. All three level
 * parameters therefore describe ln r, not r itself: theta is the
 * long-run level of ln r, kappa its mean-reversion speed and sigma its
 * volatility.
 *
 * All parameters are annualised; dt is a separate argument to the
 * process, never folded into these values.
 */
struct black_karasinski_params final {
    /// Mean-reversion speed of ln r. kappa <= 0 is a valid, if
    /// degenerate, driftless case.
    double kappa = 0.0;
    /// Long-run level of ln r, the mean-reversion target.
    double theta = 0.0;
    /// Volatility of ln r. Must be non-negative.
    double sigma = 0.0;
    /// Short rate at the start of the simulation. Must be positive: the
    /// model runs on ln r, and the logarithm of a non-positive rate is
    /// not defined.
    double initial_rate = 0.0;
};

} // namespace ores::analytics::quant::service

#endif
