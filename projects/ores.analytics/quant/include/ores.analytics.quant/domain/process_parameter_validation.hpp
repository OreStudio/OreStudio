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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_PROCESS_PARAMETER_VALIDATION_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_PROCESS_PARAMETER_VALIDATION_HPP

#include "ores.analytics.quant/export.hpp"
#include <string>
#include <vector>

namespace ores::analytics::quant::domain {

/**
 * @brief Result of validating a price-process engine's raw parameters.
 */
struct process_parameter_validation_result final {
    bool valid = true;
    std::string message; // empty when valid
};

/**
 * @brief Validate the raw (means, stdevs, weights, initial_price) tuple for the
 * given process_type ("geometric", "arithmetic", "ornstein_uhlenbeck", ...), independent of
 * both the UI and the process-construction machinery.
 *
 * This is the single source of truth for "are these parameters good?" — the
 * process classes (gaussian_mixture_model_process, ornstein_uhlenbeck_process, ...) still validate
 * defensively in their constructors, but callers that want a friendly ok/error-message pair
 * *before* attempting to build or persist a process should call this instead of duplicating the
 * rules. Lives in ores.analytics.quant (not ores.synthetic) so it's usable from any consumer
 * without pulling in a heavier service-layer library — including the Qt client.
 *
 * For a mixing engine (geometric/arithmetic): means, stdevs and weights must
 * have equal, non-empty size, every stdev must be non-negative, and the
 * weights must sum to a positive number (the server builds a
 * std::discrete_distribution from them, which is undefined behaviour over an
 * all-zero or empty set).
 *
 * For "ornstein_uhlenbeck" (single-regime, not a mixture): only stdevs.front() (sigma) and
 * weights.front() (kappa) are used; sigma must be non-negative. kappa has no
 * required sign — kappa <= 0 is a valid, if degenerate, driftless random walk.
 *
 * initial_price must be strictly positive for every engine.
 */
ORES_ANALYTICS_QUANT_EXPORT process_parameter_validation_result
validate_process_parameters(const std::string& process_type,
                            const std::vector<double>& means,
                            const std::vector<double>& stdevs,
                            const std::vector<double>& weights,
                            double initial_price);

/**
 * @brief Validate the raw (kappa, theta_path, sigma, initial_rate) tuple
 * for the given yield-curve process_type ("vasicek", "cox_ingersoll_ross", "hull_white"),
 * independent of both the UI and the process-construction machinery.
 *
 * Extends the single-source-of-truth validation above with a second
 * function rather than overloading the mean/stdev/weight-array shape onto
 * these engines' genuinely different parameter shape (a whole theta_path,
 * not a single scalar).
 *
 * For "vasicek" and "hull_white": theta_path must be non-empty; sigma must
 * be non-negative; kappa has no required sign (kappa <= 0 is a valid, if
 * degenerate, driftless case, same as "ornstein_uhlenbeck"). initial_rate has no sign
 * constraint here (a negative short rate is economically unusual but not
 * invalid for a Gaussian model).
 *
 * For "cox_ingersoll_ross": kappa and theta_path.front() (theta) must both be strictly
 * positive (the sqrt(r) volatility term makes kappa <= 0 ill-posed, and
 * Cox-Ingersoll-Ross's mean-reversion level cannot be non-positive); sigma must be
 * non-negative; initial_rate must be non-negative (Cox-Ingersoll-Ross's domain is
 * r >= 0). The Feller condition (2*kappa*theta >= sigma^2) is *not*
 * enforced here -- violating it is valid (the process can then touch
 * zero) rather than an error.
 */
ORES_ANALYTICS_QUANT_EXPORT process_parameter_validation_result
validate_yield_curve_process_parameters(const std::string& process_type,
                                        double kappa,
                                        const std::vector<double>& theta_path,
                                        double sigma,
                                        double initial_rate);

/**
 * @brief Validate the raw (mode, ticks_per_hour, branching ratio, decay) tuple
 * for an arrival process, independent of both the UI and the construction
 * machinery.
 *
 * A third function rather than another overload, for the same reason as the
 * second: an arrival process has a genuinely different parameter shape.
 *
 * The rules are the ones the tick arrival note states:
 *
 * - the mode is "periodic", "poisson" or "hawkes";
 * - the rate is positive and finite;
 * - for "hawkes", the branching ratio is at least zero and strictly below one,
 *   and the decay is positive and finite;
 * - "periodic" and "poisson" ignore the branching ratio and the decay.
 *
 * A branching ratio of one or more gives a stream that does not settle, so it
 * is refused rather than left to produce gaps that shrink without bound.
 */
ORES_ANALYTICS_QUANT_EXPORT process_parameter_validation_result validate_arrival_process_parameters(
    const std::string& mode, double ticks_per_hour, double branching_ratio, double decay);

/**
 * @brief Validate that an arrival mode and a value process can be paired.
 *
 * A periodic arrival gives every tick the same length, so any value process can
 * follow it: each step is one nominal tick. Poisson and Hawkes arrivals give
 * ticks of different lengths, so the value process must be able to take a step
 * measured in elapsed ticks, or the pair is meaningless.
 *
 * @param mode the arrival mode, as validate_arrival_process_parameters takes it.
 * @param process_supports_variable_step i_stochastic_process::supports_variable_step().
 */
ORES_ANALYTICS_QUANT_EXPORT process_parameter_validation_result
validate_step_compatibility(const std::string& mode, bool process_supports_variable_step);

}

#endif
