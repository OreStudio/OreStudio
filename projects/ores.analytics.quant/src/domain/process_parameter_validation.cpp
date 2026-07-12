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
#include "ores.analytics.quant/domain/process_parameter_validation.hpp"

namespace ores::analytics::quant::domain {

namespace {

process_parameter_validation_result invalid(std::string message) {
    return {false, std::move(message)};
}

process_parameter_validation_result validate_mixing(const std::vector<double>& means,
                                                    const std::vector<double>& stdevs,
                                                    const std::vector<double>& weights) {
    if (means.empty() || stdevs.empty() || weights.empty())
        return invalid("At least one component is required.");
    if (means.size() != stdevs.size() || means.size() != weights.size())
        return invalid("Component mean/volatility/weight counts must match.");
    for (double s : stdevs)
        if (s < 0.0)
            return invalid("Volatility (σ) cannot be negative.");
    double weightSum = 0.0;
    for (double w : weights)
        weightSum += w;
    if (weightSum <= 0.0)
        return invalid(
            "All component weights are zero. At least one component must have a positive "
            "weight.");
    return {};
}

process_parameter_validation_result validate_ou(const std::vector<double>& stdevs,
                                                const std::vector<double>& weights) {
    if (stdevs.empty() || weights.empty())
        return invalid("Ornstein-Uhlenbeck requires κ (Weight) and σ (Volatility).");
    if (stdevs.front() < 0.0)
        return invalid("Volatility (σ) cannot be negative.");
    return {};
}

}

process_parameter_validation_result validate_process_parameters(const std::string& process_type,
                                                                const std::vector<double>& means,
                                                                const std::vector<double>& stdevs,
                                                                const std::vector<double>& weights,
                                                                double initial_price) {

    if (initial_price <= 0.0)
        return invalid("Initial price must be positive.");

    if (process_type == "ou")
        return validate_ou(stdevs, weights);

    // "geometric", "arithmetic", and any unrecognised value all use the mixing
    // rule — matches process_factory::make_process's fallback-to-geometric.
    return validate_mixing(means, stdevs, weights);
}

process_parameter_validation_result
validate_yield_curve_process_parameters(const std::string& process_type,
                                        double kappa,
                                        const std::vector<double>& theta_path,
                                        double sigma,
                                        double initial_rate) {

    if (theta_path.empty())
        return invalid("theta_path must contain at least one value.");
    if (sigma < 0.0)
        return invalid("Volatility (σ) cannot be negative.");

    if (process_type == "cir") {
        if (kappa <= 0.0)
            return invalid("CIR requires κ (mean reversion speed) to be strictly positive.");
        if (theta_path.front() <= 0.0)
            return invalid("CIR requires θ (mean reversion level) to be strictly positive.");
        if (initial_rate < 0.0)
            return invalid("CIR requires a non-negative initial rate.");
        return {};
    }

    // "vasicek" and "hull_white" (Gaussian): no sign constraint on kappa or
    // initial_rate — a negative short rate is economically unusual but not
    // structurally invalid for a Gaussian model.
    return {};
}

}
