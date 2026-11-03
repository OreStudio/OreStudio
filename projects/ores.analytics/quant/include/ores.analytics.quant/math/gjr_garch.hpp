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
#ifndef ORES_ANALYTICS_QUANT_MATH_GJR_GARCH_HPP
#define ORES_ANALYTICS_QUANT_MATH_GJR_GARCH_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/mixture_tilt.hpp"
#include <expected>
#include <span>
#include <string>
#include <vector>

namespace ores::analytics::quant::math {

/**
 * @brief The reaction and carry-over of a GJR-GARCH(1,1) variance.
 *
 * sigma2(t+1) = omega + (alpha + gamma * 1{shock(t) < 0}) * shock(t)^2 + beta * sigma2(t),
 * after Glosten, Jagannathan and Runkle (1993). gamma may be negative: in FX the
 * direction that raises volatility depends on how the pair is quoted.
 */
struct gjr_garch_shape final {
    double alpha = 0.0;
    double gamma = 0.0;
    double beta = 0.0;

    friend bool operator==(const gjr_garch_shape&, const gjr_garch_shape&) = default;
};

/**
 * @brief A GJR-GARCH(1,1) variance in its admissible region.
 *
 * Built only by make(), so a model in hand always has a positive variance and a
 * finite long-run level. The long-run variance is a parameter and omega follows from
 * it, which is variance targeting (Engle and Mezrich, 1996).
 */
class ORES_ANALYTICS_QUANT_EXPORT gjr_garch_model final {
public:
    /**
     * @brief The model, or why it is outside the admissible region.
     *
     * Refuses alpha < 0, beta < 0, alpha + gamma < 0, a persistence of one or more,
     * a long-run variance that is not positive and finite, and a p_minus outside [0, 1].
     *
     * @param p_minus E[z^2 1{z < 0}] of the standardised shock, one half when it is symmetric.
     */
    static std::expected<gjr_garch_model, std::string>
    make(const gjr_garch_shape& shape, double long_run_variance, double p_minus);

    const gjr_garch_shape& shape() const noexcept {
        return shape_;
    }
    double long_run_variance() const noexcept {
        return long_run_variance_;
    }
    double p_minus() const noexcept {
        return p_minus_;
    }
    /// alpha + beta + gamma * p_minus, below one.
    double persistence() const noexcept {
        return shape_.alpha + shape_.beta + shape_.gamma * p_minus_;
    }
    double omega() const noexcept {
        return omega_;
    }

    /// The variance after a shock: the recursion.
    double next_variance(double variance, double shock) const noexcept {
        const double reaction = shock < 0.0 ? shape_.alpha + shape_.gamma : shape_.alpha;
        return omega_ + reaction * shock * shock + shape_.beta * variance;
    }

    friend bool operator==(const gjr_garch_model&, const gjr_garch_model&) = default;

private:
    gjr_garch_model(const gjr_garch_shape& shape, double long_run_variance, double p_minus);

    gjr_garch_shape shape_;
    double long_run_variance_;
    double p_minus_;
    double omega_;
};

/**
 * @brief The variances a model gives a series of shocks.
 *
 * Element 0 is the initial variance, and element t + 1 the variance after shock t,
 * so the result is one longer than the shocks and its last element is the forecast.
 */
ORES_ANALYTICS_QUANT_EXPORT std::vector<double>
filter(const gjr_garch_model& model, std::span<const double> shocks, double initial_variance);

/**
 * @brief The Gaussian quasi-likelihood cost of a series under a model.
 *
 * The mean over t of (log sigma2(t) + shock(t)^2 / sigma2(t)) / 2, with sigma2(0) the
 * initial variance: the negative log-likelihood per observation without its constant,
 * the cost QuantLib's Garch11 minimises. Infinite when a variance is not positive.
 */
ORES_ANALYTICS_QUANT_EXPORT double
mean_cost(const gjr_garch_model& model, std::span<const double> shocks, double initial_variance);

/// The mean and the variance of a mixture.
struct mixture_moments final {
    double mean;
    double variance;
};

ORES_ANALYTICS_QUANT_EXPORT mixture_moments moments_of(const mixture& m);

/**
 * @brief E[z^2 1{z < 0}] of the mixture standardised to mean zero and variance one.
 *
 * Each component contributes the second moment of its normal truncated at zero,
 * (mu^2 + s^2) Phi(-mu / s) - mu s phi(mu / s), with mu and s its standardised mean
 * and stdev. One half for any symmetric mixture.
 */
ORES_ANALYTICS_QUANT_EXPORT double negative_second_moment(const mixture& m);

/// The starts a fit begins from, when the caller names none.
ORES_ANALYTICS_QUANT_EXPORT std::vector<gjr_garch_shape> default_fit_starts();

/// The model that fits a series best, with its cost and how the search ended.
struct gjr_garch_fit final {
    gjr_garch_model model;
    double cost;
    /// How many of the starts converged; the best is taken over all of them.
    int starts_converged;
};

/**
 * @brief Fit a GJR-GARCH(1,1) to a series of returns by quasi maximum likelihood.
 *
 * The returns are demeaned, the long-run variance is their sample variance, and the
 * shape minimises mean_cost() from each start by the simplex method, the variance
 * filtered from the long-run level. The best of the starts is kept, so a start in a
 * poor basin does not decide the result. A Gaussian likelihood is consistent for the
 * shape even when the true shock is not normal, provided the mean and the variance
 * are right (Bollerslev and Wooldridge, 1992). Refuses fewer than three returns, a
 * series with no variance, and a fit that reaches no admissible point.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<gjr_garch_fit, std::string> fit_gjr_garch(
    std::span<const double> returns, double p_minus, std::span<const gjr_garch_shape> starts);

ORES_ANALYTICS_QUANT_EXPORT std::expected<gjr_garch_fit, std::string>
fit_gjr_garch(std::span<const double> returns, double p_minus);

}

#endif
