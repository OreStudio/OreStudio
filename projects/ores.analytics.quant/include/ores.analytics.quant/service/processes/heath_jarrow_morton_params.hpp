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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_HEATH_JARROW_MORTON_PARAMS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_HEATH_JARROW_MORTON_PARAMS_HPP

#include <Eigen/Dense>

namespace ores::analytics::quant::service {

/**
 * @brief Strongly-typed parameters of the heath_jarrow_morton_process.
 *
 * The row-based parameter architecture stores process parameters as
 * {name, value} pairs (see ores.synthetic's parameter definition and
 * value entities); this struct is the strongly-typed "view" of those
 * rows that the mapping layer materialises before constructing the
 * process. It is deliberately a plain aggregate -- no methods, no
 * invariants (the process constructor and the mapping layer validate).
 *
 * The process simulates a discretised forward curve: the instantaneous
 * forward rate for each fixed relative tenor tau_i, held constant over
 * each tick and updated by the single-factor HJM no-arbitrage dynamics.
 * The tenors are relative to "now" -- the curve re-anchors as time
 * passes -- so the tenors and their spacings are what define the grid,
 * not the absolute dates.
 */
struct heath_jarrow_morton_params final {
    /// The instantaneous forward rates f(0, tau_i) at the start of the
    /// simulation, one per grid point.
    Eigen::VectorXd initial_forward_rates;
    /// The per-tenor volatility sigma(T_i) of the single-factor model.
    /// Must have one entry per initial_forward_rates entry.
    Eigen::VectorXd volatilities;
    /// The grid spacing between consecutive tenors, tau_{i+1} - tau_i,
    /// including the first: the distance from the front rate (tau_0) to
    /// the first interior tenor. Each must be strictly positive; the
    /// entry count is one less than the rate count.
    Eigen::VectorXd tenor_spacings;
};

} // namespace ores::analytics::quant::service

#endif
