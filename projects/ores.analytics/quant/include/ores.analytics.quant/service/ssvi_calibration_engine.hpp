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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_SSVI_CALIBRATION_ENGINE_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_SSVI_CALIBRATION_ENGINE_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/fx_delta.hpp"
#include "ores.analytics.quant/math/fx_smile_quotes.hpp"
#include <cmath>
#include <expected>
#include <string>
#include <variant>
#include <vector>

namespace ores::analytics::quant::service {

enum class ssvi_quote_kind { atm, risk_reversal, butterfly, strike };

/// One contributor's volatility for a pillar, with the weight it carries in the fit.
struct ssvi_contribution final {
    double volatility = 0.0;
    double weight = 1.0;
};

/// A quoted point of one expiry. A pillar with no contribution counts as absent.
struct ssvi_pillar final {
    std::string point_id;
    ssvi_quote_kind kind = ssvi_quote_kind::atm;
    /// The pillar's delta, such as 0.25; read only for a risk reversal or a butterfly.
    double delta = 0.0;
    /// log(K / F); read only for a strike pillar.
    double log_moneyness = 0.0;
    std::vector<ssvi_contribution> contributions;
};

struct ssvi_expiry_quotes final {
    std::string expiry_id;
    double years = 0.0;
    math::fx_market market{};
    math::fx_quote_convention convention{};
    std::vector<ssvi_pillar> pillars;
};

struct ssvi_grid_expiry final {
    std::string expiry_id;
    double years = 0.0;
    /// The market and convention an ATM or a delta strike is resolved in. A grid of fixed
    /// log-moneyness strikes needs neither.
    math::fx_market market{};
    math::fx_quote_convention convention{};
};

/// A grid strike at a fixed log(K / F).
struct ssvi_log_moneyness_strike final {
    double value = 0.0;
};

/// The at-the-money strike of the grid expiry, in its convention.
struct ssvi_atm_strike final {};

/// The strike whose delta, at the surface's volatility there, is the stated delta.
struct ssvi_delta_strike final {
    math::fx_delta_type type = math::fx_delta_type::spot;
    math::option_side side = math::option_side::call;
    /// The size of the delta, such as 0.25.
    double delta = 0.0;
};

struct ssvi_grid_strike final {
    std::string strike_id;
    std::variant<ssvi_log_moneyness_strike, ssvi_atm_strike, ssvi_delta_strike> at;
};

struct ssvi_request final {
    /// In strictly increasing order of years.
    std::vector<ssvi_expiry_quotes> expiries;
    std::vector<ssvi_grid_expiry> grid_expiries;
    std::vector<ssvi_grid_strike> grid_strikes;
    /// The largest absolute error of a pillar, in volatility, that is not reported.
    double pillar_tolerance = 0.0;
};

struct ssvi_expiry_fit final {
    std::string expiry_id;
    double years = 0.0;
    double atm_total_variance = 0.0;
};

/// A power-law SSVI surface, so phi(theta) = eta * theta^(-gamma), with gamma in (0, 1).
struct ssvi_parameters final {
    double rho = 0.0;
    double eta = 0.0;
    double gamma = 0.5;
    std::vector<ssvi_expiry_fit> expiries;
};

struct ssvi_pillar_fit final {
    std::string point_id;
    std::string expiry_id;
    /// The weighted mean of the contributions.
    double quoted = 0.0;
    double fitted = 0.0;
    double error = 0.0;
    /// The strikes the label resolved to on the fitted smile: one for an ATM or a strike, put then
    /// call for a wing.
    std::vector<double> strikes;
    bool beyond_tolerance = false;
};

struct ssvi_grid_point final {
    std::string expiry_id;
    std::string strike_id;
    /// log(K / F) the strike resolved to at the expiry.
    double log_moneyness = 0.0;
    double total_variance = 0.0;
    double volatility = 0.0;
};

enum class ssvi_arbitrage_kind { calendar, butterfly };

struct ssvi_violation final {
    ssvi_arbitrage_kind kind = ssvi_arbitrage_kind::calendar;
    std::string expiry_id;
    std::string description;
};

struct ssvi_calibration final {
    ssvi_parameters parameters;
    std::vector<ssvi_pillar_fit> pillars;
    std::vector<ssvi_grid_point> grid;
    std::vector<ssvi_violation> violations;
    /// Whether the search over rho, eta and gamma stopped within its tolerance, with no theta
    /// held at the bound of its own search.
    bool converged = false;
};

/// The curvature of a power-law SSVI slice.
[[nodiscard]] inline double ssvi_phi(double theta, double eta, double gamma) noexcept {
    return eta * std::pow(theta, -gamma);
}

/// The total implied variance of an SSVI slice at log-moneyness k, given its phi.
[[nodiscard]] inline double
ssvi_total_variance(double k, double theta, double rho, double phi) noexcept {
    const double x = phi * k + rho;
    return 0.5 * theta * (1.0 + rho * phi * k + std::sqrt(x * x + 1.0 - rho * rho));
}

/**
 * @brief The conditions of Gatheral and Jacquier (2014), Corollary 4.1, that a surface breaks.
 *
 * Condition 2 is not listed: for a power law with gamma in (0, 1) it holds for every rho.
 */
ORES_ANALYTICS_QUANT_EXPORT std::vector<ssvi_violation>
ssvi_violations(const ssvi_parameters& parameters);

/**
 * @brief Fits SSVI to the weighted quotes of every expiry and samples the output grid.
 *
 * The fit minimises the weighted squared difference between the quotes and the surface's own
 * quotes in each expiry's convention. Given rho, eta and gamma the expiries are independent, so
 * each theta is solved on its own inside a search over the three shared parameters. The fit does
 * not hold the surface inside the no-arbitrage region: it reports what the quotes imply, in the
 * violations of the result. The violations cover the quoted expiries. A grid expiry takes its
 * theta linearly between them, so it is calendar-free whenever they are.
 *
 * An ATM or a delta grid strike is solved on the fitted smile of its grid expiry, in that
 * expiry's market and convention.
 *
 * Refuses an expiry with neither an ATM and a risk reversal and butterfly at one delta, nor
 * three strike pillars, naming the expiry and what it lacks, a grid expiry outside the quoted
 * expiries, and an ATM or a delta grid strike at a grid expiry with no market.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<ssvi_calibration, std::string>
calibrate_ssvi(const ssvi_request& request);

}

#endif
