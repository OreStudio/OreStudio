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
#include "ores.analytics.quant/service/ssvi_calibration_engine.hpp"
#include "ores.analytics.quant/math/nelder_mead.hpp"
#include <boost/math/tools/minima.hpp>
#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <limits>
#include <ranges>
#include <span>
#include <utility>
#include <vector>

namespace ores::analytics::quant::service {

namespace {

using math::fx_atm_reading;
using math::fx_wing_reading;

/// The objective of a point the convention cannot read, far above any real squared error.
constexpr double unreadable_penalty = 1e6;
/// How far a theta may move from the ATM quote's, as a factor either way.
constexpr double theta_search_factor = 4.0;
/// The share of the search's reach beyond which a theta counts as stopped by the bound.
constexpr double theta_bound_share = 1.0 - 1e-6;
constexpr int theta_search_bits = std::numeric_limits<double>::digits / 2;
constexpr std::uintmax_t theta_search_iterations = 100;
constexpr math::nelder_mead_options shared_search{.tolerance = 1e-8, .max_iterations = 1000};
constexpr std::int32_t atm_wing = -1;
/// Keeps gamma strictly inside (0, 1), as QuantExt's power-law bounds do.
constexpr double gamma_margin = 1e-6;

/// What every slice of the surface shares.
struct fit_shape final {
    double rho;
    double eta;
    double gamma;
};

fit_shape shape_of(const std::array<double, 3>& u) noexcept {
    const double unit = 0.5 * (1.0 + std::tanh(u[2]));
    return {.rho = std::tanh(u[0]),
            .eta = std::exp(u[1]),
            .gamma = gamma_margin + unit * (1.0 - 2.0 * gamma_margin)};
}

/// A pillar as the objective reads it; its strings stay in the request.
struct fit_pillar final {
    double quoted;
    double weight;
    double log_moneyness;
    std::int32_t wing;
    ssvi_quote_kind kind;
};

struct fit_expiry final {
    double years;
    double start_theta;
    math::fx_market market;
    math::fx_quote_convention convention;
    std::uint32_t first_pillar;
    std::uint32_t pillar_count;
    std::uint32_t first_delta;
    std::uint32_t delta_count;
};

/// The whole fit in flat rows, with one wing buffer, so the objective allocates nothing.
struct fit_state final {
    std::vector<fit_expiry> expiries;
    std::vector<fit_pillar> pillars;
    /// Parallel to pillars, read only when the result is written.
    std::vector<const ssvi_pillar*> sources;
    std::vector<double> deltas;
    std::vector<fx_wing_reading> wings;

    [[nodiscard]] std::span<const fit_pillar> pillars_of(const fit_expiry& e) const noexcept {
        return std::span(pillars).subspan(e.first_pillar, e.pillar_count);
    }
    [[nodiscard]] std::span<const double> deltas_of(const fit_expiry& e) const noexcept {
        return std::span(deltas).subspan(e.first_delta, e.delta_count);
    }
};

/// One slice as a pillar reads it, with phi computed once.
struct fit_slice final {
    double theta;
    double rho;
    double phi;
    double years;

    [[nodiscard]] double volatility(double k) const noexcept {
        return std::sqrt(ssvi_total_variance(k, theta, rho, phi) / years);
    }
};

fit_slice slice_of(const fit_expiry& expiry, double theta, const fit_shape& shape) noexcept {
    return {.theta = theta,
            .rho = shape.rho,
            .phi = ssvi_phi(theta, shape.eta, shape.gamma),
            .years = expiry.years};
}

std::expected<fx_atm_reading, std::string>
read(fit_state& state, const fit_expiry& expiry, const fit_slice& slice) {
    return math::read_smile([&](double k) { return slice.volatility(k); },
                            std::sqrt(slice.theta / slice.years),
                            slice.years,
                            expiry.market,
                            expiry.convention,
                            state.deltas_of(expiry),
                            state.wings);
}

double model_value(const fit_pillar& pillar,
                   const fit_slice& slice,
                   const fx_atm_reading& atm,
                   std::span<const fx_wing_reading> wings) noexcept {
    switch (pillar.kind) {
        case ssvi_quote_kind::strike:
            return slice.volatility(pillar.log_moneyness);
        case ssvi_quote_kind::atm:
            return atm.volatility;
        case ssvi_quote_kind::risk_reversal:
            return wings[static_cast<std::size_t>(pillar.wing)].risk_reversal;
        case ssvi_quote_kind::butterfly:
            return wings[static_cast<std::size_t>(pillar.wing)].butterfly;
    }
    return std::numeric_limits<double>::quiet_NaN();
}

double
squared_error(fit_state& state, const fit_expiry& expiry, double theta, const fit_shape& shape) {
    const auto slice = slice_of(expiry, theta, shape);
    const auto atm = read(state, expiry, slice);
    if (!atm)
        return unreadable_penalty;
    double sum = 0.0;
    for (const auto& pillar : state.pillars_of(expiry)) {
        const double residual = model_value(pillar, slice, *atm, state.wings) - pillar.quoted;
        sum += pillar.weight * residual * residual;
    }
    return std::isfinite(sum) ? sum : unreadable_penalty;
}

struct theta_fit final {
    double theta;
    double value;
    bool at_bound;
};

theta_fit fit_theta(fit_state& state, const fit_expiry& expiry, const fit_shape& shape) {
    std::uintmax_t iterations = theta_search_iterations;
    const double centre = std::log(expiry.start_theta);
    const double reach = std::log(theta_search_factor);
    const auto [log_theta, value] = boost::math::tools::brent_find_minima(
        [&](double y) { return squared_error(state, expiry, std::exp(y), shape); },
        centre - reach,
        centre + reach,
        theta_search_bits,
        iterations);
    return {std::exp(log_theta), value, std::abs(log_theta - centre) >= reach * theta_bound_share};
}

/// The strike pillars that fix a slice's three degrees of freedom without an ATM and a wing.
constexpr int enough_strike_pillars = 3;

std::string missing_pillars(bool has_atm, bool has_wing) {
    if (!has_atm && !has_wing)
        return "an ATM and a risk reversal and butterfly at one delta";
    return has_atm ? "a risk reversal and butterfly at one delta" : "an ATM";
}

std::expected<void, std::string> append(fit_state& state, const ssvi_expiry_quotes& expiry) {
    if (!(expiry.years > 0.0) || !std::isfinite(expiry.years))
        return std::unexpected("expiry " + expiry.expiry_id + " needs a positive time");
    if (!(expiry.market.forward() > 0.0) || !std::isfinite(expiry.market.forward()))
        return std::unexpected("expiry " + expiry.expiry_id + " needs a positive forward");

    const auto first_pillar = state.pillars.size();
    const auto first_delta = state.deltas.size();
    double start_theta = 0.0;
    bool has_atm = false;
    int strike_pillars = 0;
    double nearest_strike = std::numeric_limits<double>::infinity();
    std::vector<double> risk_reversals;
    std::vector<double> butterflies;
    for (const auto& pillar : expiry.pillars) {
        if (pillar.contributions.empty())
            continue;
        double weight = 0.0;
        double weighted = 0.0;
        for (const auto& [volatility, contribution_weight] : pillar.contributions) {
            if (!(contribution_weight > 0.0) || !std::isfinite(volatility))
                return std::unexpected("pillar " + pillar.point_id +
                                       " needs finite quotes with positive weights");
            weight += contribution_weight;
            weighted += contribution_weight * volatility;
        }
        const double quoted = weighted / weight;
        state.pillars.push_back({.quoted = quoted,
                                 .weight = weight,
                                 .log_moneyness = pillar.log_moneyness,
                                 .wing = atm_wing,
                                 .kind = pillar.kind});
        state.sources.push_back(&pillar);
        if (pillar.kind == ssvi_quote_kind::atm) {
            if (!(quoted > 0.0))
                return std::unexpected("pillar " + pillar.point_id + " needs a positive ATM");
            has_atm = true;
            start_theta = quoted * quoted * expiry.years;
            continue;
        }
        if (pillar.kind == ssvi_quote_kind::strike) {
            if (!(quoted > 0.0) || !std::isfinite(pillar.log_moneyness))
                return std::unexpected("pillar " + pillar.point_id +
                                       " needs a positive volatility at a finite strike");
            ++strike_pillars;
            if (!has_atm && std::abs(pillar.log_moneyness) < nearest_strike) {
                nearest_strike = std::abs(pillar.log_moneyness);
                start_theta = quoted * quoted * expiry.years;
            }
            continue;
        }
        if (!(pillar.delta > 0.0) || !(pillar.delta < 0.5))
            return std::unexpected("pillar " + pillar.point_id + " needs a delta below one half");
        state.deltas.push_back(pillar.delta);
        (pillar.kind == ssvi_quote_kind::risk_reversal ? risk_reversals : butterflies)
            .push_back(pillar.delta);
    }
    const bool has_wing = std::ranges::any_of(risk_reversals, [&](double d) {
        return std::ranges::find(butterflies, d) != butterflies.end();
    });
    if ((!has_atm || !has_wing) && strike_pillars < enough_strike_pillars)
        return std::unexpected("expiry " + expiry.expiry_id + " lacks " +
                               missing_pillars(has_atm, has_wing));

    const auto own_deltas = std::ranges::subrange(
        state.deltas.begin() + static_cast<std::ptrdiff_t>(first_delta), state.deltas.end());
    std::ranges::sort(own_deltas);
    const auto repeats = std::ranges::unique(own_deltas);
    state.deltas.erase(repeats.begin(), repeats.end());
    const auto delta_count = state.deltas.size() - first_delta;
    const auto deltas = std::span(state.deltas).subspan(first_delta, delta_count);
    for (auto i = first_pillar; i < state.pillars.size(); ++i) {
        if (state.pillars[i].kind == ssvi_quote_kind::risk_reversal ||
            state.pillars[i].kind == ssvi_quote_kind::butterfly)
            state.pillars[i].wing = static_cast<std::int32_t>(
                std::ranges::lower_bound(deltas, state.sources[i]->delta) - deltas.begin());
    }
    state.expiries.push_back(
        {.years = expiry.years,
         .start_theta = start_theta,
         .market = expiry.market,
         .convention = expiry.convention,
         .first_pillar = static_cast<std::uint32_t>(first_pillar),
         .pillar_count = static_cast<std::uint32_t>(state.pillars.size() - first_pillar),
         .first_delta = static_cast<std::uint32_t>(first_delta),
         .delta_count = static_cast<std::uint32_t>(delta_count)});
    state.wings.resize(std::max(state.wings.size(), delta_count));
    return {};
}

std::expected<double, std::string> interpolate_theta(const ssvi_parameters& parameters,
                                                     const ssvi_grid_expiry& at) {
    const auto& fits = parameters.expiries;
    if (!(at.years >= fits.front().years && at.years <= fits.back().years))
        return std::unexpected("grid expiry " + at.expiry_id + " lies outside the quoted expiries");
    const auto upper = std::ranges::lower_bound(fits, at.years, {}, &ssvi_expiry_fit::years);
    if (upper->years == at.years)
        return upper->atm_total_variance;
    const auto lower = std::prev(upper);
    const double share = (at.years - lower->years) / (upper->years - lower->years);
    return lower->atm_total_variance +
           share * (upper->atm_total_variance - lower->atm_total_variance);
}

std::expected<double, std::string> grid_log_moneyness(const ssvi_grid_expiry& expiry,
                                                      const ssvi_grid_strike& strike,
                                                      double theta,
                                                      double rho,
                                                      double phi) {
    if (const auto* fixed = std::get_if<ssvi_log_moneyness_strike>(&strike.at))
        return fixed->value;
    if (!(expiry.market.spot > 0.0))
        return std::unexpected("grid expiry " + expiry.expiry_id +
                               " has no market to resolve strike " + strike.strike_id);
    const auto smile = [&](double k) {
        return std::sqrt(ssvi_total_variance(k, theta, rho, phi) / expiry.years);
    };
    const double level = std::sqrt(theta / expiry.years);
    const auto resolved =
        std::holds_alternative<ssvi_atm_strike>(strike.at) ?
            math::smile_atm_log_moneyness(smile,
                                          level,
                                          expiry.years,
                                          expiry.market,
                                          expiry.convention.atm,
                                          expiry.convention.delta) :
            math::smile_delta_log_moneyness(smile,
                                            level,
                                            expiry.years,
                                            expiry.market,
                                            std::get<ssvi_delta_strike>(strike.at).type,
                                            std::get<ssvi_delta_strike>(strike.at).side,
                                            std::get<ssvi_delta_strike>(strike.at).delta);
    if (!resolved)
        return std::unexpected("strike " + strike.strike_id + " at grid expiry " +
                               expiry.expiry_id + ": " + resolved.error());
    return *resolved;
}

std::vector<double> strikes_of(const fit_pillar& pillar,
                               double forward,
                               const fx_atm_reading& atm,
                               std::span<const fx_wing_reading> wings) {
    if (pillar.kind == ssvi_quote_kind::strike)
        return {forward * std::exp(pillar.log_moneyness)};
    if (pillar.kind == ssvi_quote_kind::atm)
        return {atm.strike};
    const auto& wing = wings[static_cast<std::size_t>(pillar.wing)];
    return {wing.put_strike, wing.call_strike};
}

}

std::vector<ssvi_violation> ssvi_violations(const ssvi_parameters& parameters) {
    std::vector<ssvi_violation> out;
    const double skew_factor = 1.0 + std::abs(parameters.rho);
    const auto& fits = parameters.expiries;
    for (std::size_t i = 0; i < fits.size(); ++i) {
        const double theta = fits[i].atm_total_variance;
        const double phi = ssvi_phi(theta, parameters.eta, parameters.gamma);
        if (i > 0 && theta < fits[i - 1].atm_total_variance)
            out.push_back(
                {.kind = ssvi_arbitrage_kind::calendar,
                 .expiry_id = fits[i].expiry_id,
                 .description = "ATM total variance falls from expiry " + fits[i - 1].expiry_id});
        if (theta * phi * skew_factor >= 4.0)
            out.push_back({.kind = ssvi_arbitrage_kind::butterfly,
                           .expiry_id = fits[i].expiry_id,
                           .description = "theta phi(theta) (1 + |rho|) reaches 4"});
        if (theta * phi * phi * skew_factor > 4.0)
            out.push_back({.kind = ssvi_arbitrage_kind::butterfly,
                           .expiry_id = fits[i].expiry_id,
                           .description = "theta phi(theta)^2 (1 + |rho|) exceeds 4"});
    }
    return out;
}

std::expected<ssvi_calibration, std::string> calibrate_ssvi(const ssvi_request& request) {
    if (request.expiries.empty())
        return std::unexpected("the calibration needs at least one expiry");
    fit_state state;
    state.expiries.reserve(request.expiries.size());
    for (const auto& expiry : request.expiries) {
        if (!state.expiries.empty() && !(expiry.years > state.expiries.back().years))
            return std::unexpected("expiry " + expiry.expiry_id +
                                   " is not after the expiry before it");
        if (auto appended = append(state, expiry); !appended)
            return std::unexpected(appended.error());
    }

    const auto objective = [&](const std::array<double, 3>& u) {
        const auto shape = shape_of(u);
        double sum = 0.0;
        for (const auto& expiry : state.expiries)
            sum += fit_theta(state, expiry, shape).value;
        return sum;
    };
    const auto shared =
        math::nelder_mead<3>(objective, {0.0, std::log(0.5), 0.0}, {0.5, 0.5, 0.5}, shared_search);

    ssvi_calibration out;
    out.converged = shared.converged;
    const auto shape = shape_of(shared.x);
    out.parameters.rho = shape.rho;
    out.parameters.eta = shape.eta;
    out.parameters.gamma = shape.gamma;
    out.parameters.expiries.reserve(state.expiries.size());
    out.pillars.reserve(state.pillars.size());
    for (std::size_t e = 0; e < state.expiries.size(); ++e) {
        const auto& expiry = state.expiries[e];
        const auto& source = request.expiries[e];
        const auto fitted_theta = fit_theta(state, expiry, shape);
        const double theta = fitted_theta.theta;
        out.converged = out.converged && !fitted_theta.at_bound;
        out.parameters.expiries.push_back(
            {.expiry_id = source.expiry_id, .years = expiry.years, .atm_total_variance = theta});
        const auto slice = slice_of(expiry, theta, shape);
        const auto atm = read(state, expiry, slice);
        if (!atm)
            return std::unexpected("expiry " + source.expiry_id +
                                   " cannot be read on its fitted smile: " + atm.error());
        const auto pillars = state.pillars_of(expiry);
        for (std::size_t p = 0; p < pillars.size(); ++p) {
            const double fitted = model_value(pillars[p], slice, *atm, state.wings);
            const double error = fitted - pillars[p].quoted;
            out.pillars.push_back(
                {.point_id = state.sources[expiry.first_pillar + p]->point_id,
                 .expiry_id = source.expiry_id,
                 .quoted = pillars[p].quoted,
                 .fitted = fitted,
                 .error = error,
                 .strikes = strikes_of(pillars[p], expiry.market.forward(), *atm, state.wings),
                 .beyond_tolerance = std::abs(error) > request.pillar_tolerance});
        }
    }

    for (const auto& grid_expiry : request.grid_expiries) {
        const auto theta = interpolate_theta(out.parameters, grid_expiry);
        if (!theta)
            return std::unexpected(theta.error());
        const double phi = ssvi_phi(*theta, shape.eta, shape.gamma);
        for (const auto& strike : request.grid_strikes) {
            const auto k = grid_log_moneyness(grid_expiry, strike, *theta, shape.rho, phi);
            if (!k)
                return std::unexpected(k.error());
            const double w = ssvi_total_variance(*k, *theta, shape.rho, phi);
            out.grid.push_back({.expiry_id = grid_expiry.expiry_id,
                                .strike_id = strike.strike_id,
                                .log_moneyness = *k,
                                .total_variance = w,
                                .volatility = std::sqrt(w / grid_expiry.years)});
        }
    }
    out.violations = ssvi_violations(out.parameters);
    return out;
}

}
