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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_BLACK_DERMAN_TOY_PARAMS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_BLACK_DERMAN_TOY_PARAMS_HPP

#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief Strongly-typed parameters of the black_derman_toy_process.
 *
 * The row-based parameter architecture stores process parameters as
 * {name, value} pairs (see ores.synthetic's parameter definition and
 * value entities); this struct is the strongly-typed "view" of those
 * rows that the mapping layer materialises before constructing the
 * process. It is deliberately a plain aggregate -- no methods, no
 * invariants (the process constructor and the mapping layer validate).
 *
 * The Black-Derman-Toy model is a lattice, not a stochastic
 * differential equation: the tree is calibrated to reproduce the
 * input discount curve exactly, and the curve is therefore a model
 * input, not an output. sigma_path carries the time-varying
 * volatility of ln r, one value per tick, held flat once it runs out
 * (the same flat-extension convention as hull_white_process's
 * theta_path).
 *
 * All parameters are annualised; dt is a separate argument to the
 * process, never folded into these values.
 */
struct black_derman_toy_params final {
    /// The discount curve D(t_i) the tree must reproduce: one factor
    /// per tick, strictly decreasing, each in (0, 1).
    std::vector<double> discount_curve;
    /// Time-varying volatility of ln r, one value per tick (held flat
    /// past the end of the path). Must be non-empty and non-negative.
    std::vector<double> sigma_path;
};

} // namespace ores::analytics::quant::service

#endif
