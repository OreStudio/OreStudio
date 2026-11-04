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
#include "ores.analytics.quant/math/gjr_garch.hpp"
#include "ores.analytics.quant/math/nelder_mead.hpp"
#include "ores.analytics.quant/math/normal.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <limits>
#include <numbers>
#include <optional>

namespace ores::analytics::quant::math {

namespace {

/// The cost of a shape outside the admissible region, large enough that the simplex leaves it.
constexpr double inadmissible_shape_cost = 1e10;

}

std::expected<gjr_garch_model, std::string>
gjr_garch_model::make(const gjr_garch_shape& shape, double long_run_variance, double p_minus) {
    const auto finite = [](double v) {
        return std::isfinite(v);
    };
    if (!finite(shape.alpha) || !finite(shape.gamma) || !finite(shape.beta))
        return std::unexpected("the GJR-GARCH parameters must be finite");
    if (shape.alpha < 0.0)
        return std::unexpected("alpha must not be negative");
    if (shape.beta < 0.0)
        return std::unexpected("beta must not be negative");
    if (shape.alpha + shape.gamma < 0.0)
        return std::unexpected(
            "alpha + gamma must not be negative, or a fall would lower the variance");
    if (!(p_minus >= 0.0 && p_minus <= 1.0))
        return std::unexpected("p_minus must lie in [0, 1]");
    if (!(long_run_variance > 0.0) || !finite(long_run_variance))
        return std::unexpected("the long-run variance must be positive and finite");
    const double persistence = shape.alpha + shape.beta + shape.gamma * p_minus;
    if (!(persistence < 1.0))
        return std::unexpected("the persistence alpha + beta + gamma * p_minus must be below one");
    return gjr_garch_model(shape, long_run_variance, p_minus);
}

bool gjr_garch_model::admissible(const gjr_garch_shape& shape, double p_minus) noexcept {
    return std::isfinite(shape.alpha) && std::isfinite(shape.gamma) && std::isfinite(shape.beta) &&
           shape.alpha >= 0.0 && shape.beta >= 0.0 && shape.alpha + shape.gamma >= 0.0 &&
           p_minus >= 0.0 && p_minus <= 1.0 &&
           shape.alpha + shape.beta + shape.gamma * p_minus < 1.0;
}

gjr_garch_model::gjr_garch_model(const gjr_garch_shape& shape,
                                 double long_run_variance,
                                 double p_minus)
    : shape_(shape)
    , long_run_variance_(long_run_variance)
    , p_minus_(p_minus)
    , omega_(long_run_variance * (1.0 - (shape.alpha + shape.beta + shape.gamma * p_minus))) {}

std::vector<double>
filter(const gjr_garch_model& model, std::span<const double> shocks, double initial_variance) {
    std::vector<double> out;
    out.reserve(shocks.size() + 1);
    double variance = initial_variance;
    out.push_back(variance);
    for (const double shock : shocks) {
        variance = model.next_variance(variance, shock);
        out.push_back(variance);
    }
    return out;
}

double
mean_cost(const gjr_garch_model& model, std::span<const double> shocks, double initial_variance) {
    if (shocks.empty())
        return 0.0;
    double variance = initial_variance;
    double total = 0.0;
    for (const double shock : shocks) {
        if (!(variance > 0.0))
            return std::numeric_limits<double>::infinity();
        total += std::log(variance) + shock * shock / variance;
        variance = model.next_variance(variance, shock);
    }
    return total / (2.0 * static_cast<double>(shocks.size()));
}

mixture_moments moments_of(const mixture& m) {
    double mean = 0.0;
    for (std::size_t k = 0; k < m.means.size(); ++k)
        mean += m.weights[k] * m.means[k];
    double variance = 0.0;
    for (std::size_t k = 0; k < m.means.size(); ++k) {
        const double d = m.means[k] - mean;
        variance += m.weights[k] * (m.stdevs[k] * m.stdevs[k] + d * d);
    }
    return {.mean = mean, .variance = variance};
}

double negative_second_moment(const mixture& m) {
    const auto [mean, variance] = moments_of(m);
    if (!(variance > 0.0))
        return 0.5;
    const double scale = std::sqrt(variance);
    double total = 0.0;
    double weight = 0.0;
    for (std::size_t k = 0; k < m.means.size(); ++k) {
        const double mu = (m.means[k] - mean) / scale;
        const double s = m.stdevs[k] / scale;
        weight += m.weights[k];
        if (s > 0.0)
            total += m.weights[k] *
                     ((mu * mu + s * s) * normal_cdf(-mu / s) - mu * s * normal_pdf(mu / s));
        else if (mu < 0.0)
            total += m.weights[k] * mu * mu;
    }
    return weight > 0.0 ? total / weight : 0.5;
}

double daily_negative_second_moment(const mixture& m, double ticks_per_day) {
    if (!(ticks_per_day > 1.0))
        return negative_second_moment(m);
    const auto [mean, variance] = moments_of(m);
    if (!(variance > 0.0))
        return 0.5;
    double third = 0.0;
    for (std::size_t k = 0; k < m.means.size(); ++k) {
        const double d = m.means[k] - mean;
        third += m.weights[k] * (d * d * d + 3.0 * d * m.stdevs[k] * m.stdevs[k]);
    }
    const double daily_skew = third / (variance * std::sqrt(variance)) / std::sqrt(ticks_per_day);
    const double p = 0.5 - daily_skew / (3.0 * std::sqrt(2.0 * std::numbers::pi));
    return std::clamp(p, 0.0, 1.0);
}

std::vector<gjr_garch_shape> default_fit_starts() {
    return {{.alpha = 0.05, .gamma = 0.0, .beta = 0.90},
            {.alpha = 0.10, .gamma = 0.0, .beta = 0.80},
            {.alpha = 0.03, .gamma = 0.0, .beta = 0.95},
            {.alpha = 0.04, .gamma = 0.06, .beta = 0.88},
            {.alpha = 0.08, .gamma = -0.06, .beta = 0.88}};
}

std::expected<gjr_garch_fit, std::string> fit_gjr_garch(std::span<const double> returns,
                                                        double p_minus,
                                                        std::span<const gjr_garch_shape> starts) {
    if (returns.size() < 3)
        return std::unexpected("a GJR-GARCH fit needs at least three returns");
    if (starts.empty())
        return std::unexpected("a GJR-GARCH fit needs at least one start");

    double mean = 0.0;
    for (const double r : returns)
        mean += r;
    mean /= static_cast<double>(returns.size());
    std::vector<double> shocks;
    shocks.reserve(returns.size());
    double variance = 0.0;
    for (const double r : returns) {
        shocks.push_back(r - mean);
        variance += (r - mean) * (r - mean);
    }
    variance /= static_cast<double>(returns.size());
    // Demeaning a constant series leaves rounding residue, not variance: a spread a
    // trillionth of the mean's size is no movement to fit.
    if (!std::isfinite(variance) || !(variance > 1e-24 + 1e-24 * mean * mean))
        return std::unexpected("the returns have no variance to fit");

    const auto cost_of = [&](const std::array<double, 3>& x) {
        const gjr_garch_shape shape{.alpha = x[0], .gamma = x[1], .beta = x[2]};
        if (!gjr_garch_model::admissible(shape, p_minus))
            return inadmissible_shape_cost;
        const auto model = gjr_garch_model::make(shape, variance, p_minus);
        if (!model)
            return inadmissible_shape_cost;
        const double c = mean_cost(*model, shocks, variance);
        return std::isfinite(c) ? c : inadmissible_shape_cost;
    };

    std::optional<gjr_garch_fit> best;
    int converged = 0;
    std::vector<double> reached;
    reached.reserve(starts.size());
    for (const auto& start : starts) {
        const auto result =
            nelder_mead<3>(cost_of, {start.alpha, start.gamma, start.beta}, {0.02, 0.02, 0.02});
        if (result.converged)
            ++converged;
        if (result.value >= inadmissible_shape_cost)
            continue;
        const auto model = gjr_garch_model::make(
            {.alpha = result.x[0], .gamma = result.x[1], .beta = result.x[2]}, variance, p_minus);
        if (!model)
            continue;
        reached.push_back(result.value);
        if (!best || result.value < best->cost)
            best = gjr_garch_fit{.model = *model, .cost = result.value, .starts_converged = 0};
    }
    if (!best)
        return std::unexpected("no start of the GJR-GARCH fit reached an admissible point");
    best->starts_converged = converged;
    best->starts_tried = static_cast<int>(starts.size());
    best->starts_agreed = std::ranges::all_of(
        reached, [&](double c) { return c - best->cost <= fit_starts_agreement; });
    return *best;
}

std::expected<gjr_garch_fit, std::string> fit_gjr_garch(std::span<const double> returns,
                                                        double p_minus) {
    const auto starts = default_fit_starts();
    return fit_gjr_garch(returns, p_minus, starts);
}

}
