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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_COX_INGERSOLL_ROSS_PARAMS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_COX_INGERSOLL_ROSS_PARAMS_HPP

namespace ores::analytics::quant::service {

/**
 * @brief Strongly-typed parameters of the cox_ingersoll_ross_process.
 *
 * The row-based parameter architecture stores process parameters as
 * {name, value} pairs (see ores.synthetic's parameter definition and
 * value entities); this struct is the strongly-typed "view" of those
 * rows that the mapping layer materialises before constructing the
 * process. It is deliberately a plain aggregate -- no methods, no
 * invariants (the process constructor and the mapping layer validate).
 *
 * All parameters are annualised; dt is a separate argument to the
 * process, never folded into these values.
 */
struct cox_ingersoll_ross_params final {
    /// Mean-reversion speed. Must be strictly positive (the sqrt(r)
    /// volatility term makes kappa <= 0 ill-posed).
    double kappa = 0.0;
    /// Long-term mean-reversion level. Must be strictly positive.
    double theta = 0.0;
    /// Volatility. Must be non-negative; sigma == 0 degenerates to the
    /// deterministic mean-reversion ODE.
    double sigma = 0.0;
    /// Short rate at the start of the simulation. Must be non-negative
    /// (the process's domain is r >= 0).
    double initial_rate = 0.0;
};

} // namespace ores::analytics::quant::service

#endif
