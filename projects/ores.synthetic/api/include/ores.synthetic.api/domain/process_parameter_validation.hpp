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
#ifndef ORES_SYNTHETIC_API_DOMAIN_PROCESS_PARAMETER_VALIDATION_HPP
#define ORES_SYNTHETIC_API_DOMAIN_PROCESS_PARAMETER_VALIDATION_HPP

#include "ores.synthetic.api/export.hpp"
#include <string>
#include <vector>

namespace ores::synthetic::domain {

/**
 * @brief Result of validating a price-process engine's raw parameters.
 */
struct process_parameter_validation_result final {
    bool valid = true;
    std::string message; // empty when valid
};

/**
 * @brief Validate the raw (means, stdevs, weights, initial_price) tuple for the
 * given process_type ("geometric", "arithmetic", "ou", ...), independent of
 * both the UI and the process-construction machinery.
 *
 * This is the single source of truth for "are these parameters good?" — the
 * process classes in ores.synthetic.service (gmm_process, ou_process, ...)
 * still validate defensively in their constructors, but callers that want a
 * friendly ok/error-message pair *before* attempting to build or persist a
 * process should call this instead of duplicating the rules. Lives in
 * ores.synthetic.api (not .service/.core) so it's usable from the Qt client,
 * which cannot link the heavier service-layer libraries.
 *
 * For a mixing engine (geometric/arithmetic): means, stdevs and weights must
 * have equal, non-empty size, every stdev must be non-negative, and the
 * weights must sum to a positive number (the server builds a
 * std::discrete_distribution from them, which is undefined behaviour over an
 * all-zero or empty set).
 *
 * For "ou" (single-regime, not a mixture): only stdevs.front() (sigma) and
 * weights.front() (kappa) are used; sigma must be non-negative. kappa has no
 * required sign — kappa <= 0 is a valid, if degenerate, driftless random walk.
 *
 * initial_price must be strictly positive for every engine.
 */
ORES_SYNTHETIC_API_EXPORT process_parameter_validation_result
validate_process_parameters(const std::string& process_type,
                            const std::vector<double>& means,
                            const std::vector<double>& stdevs,
                            const std::vector<double>& weights,
                            double initial_price);

}

#endif
