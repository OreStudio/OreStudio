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
#include "ores.analytics.quant/service/fx_vol_surface_generator.hpp"
#include "ores.analytics.quant/math/normal.hpp"
#include <algorithm>
#include <cmath>
#include <format>
#include <limits>
#include <numbers>
#include <optional>
#include <random>
#include <stdexcept>
#include <utility>

namespace ores::analytics::quant::service {

namespace {

constexpr double fixed_gamma = 0.5;
constexpr double curvature_bound = std::numbers::sqrt2;

/// A forward variance never reaches zero, so the ATM total variance keeps rising.
constexpr double variance_floor_fraction = 1e-6;

/// The exact step of an Ornstein-Uhlenbeck process, so a large step keeps the stationary spread.
double step_ou(double x, double level, double reversion, double volatility, double dt, double z) {
    if (reversion <= 0.0)
        return x + volatility * std::sqrt(dt) * z;
    const double decay = std::exp(-reversion * dt);
    const double spread = volatility * std::sqrt((1.0 - decay * decay) / (2.0 * reversion));
    return level + (x - level) * decay + spread * z;
}

/// Whether x is a finite number of at least zero; a NaN is neither.
bool non_negative(double x) noexcept {
    return std::isfinite(x) && x >= 0.0;
}

std::expected<void, std::string> check_dynamics(const fx_vol_surface_dynamics& d,
                                                std::size_t segments) {
    if (d.variance_reversion.size() != segments || d.variance_volatility.size() != segments)
        return std::unexpected(std::format(
            "The dynamics name {} reversion(s) and {} volatility(ies) of the forward variance, and "
            "the surface has {} segment(s).",
            d.variance_reversion.size(),
            d.variance_volatility.size(),
            segments));
    for (std::size_t j = 0; j < segments; ++j)
        if (!non_negative(d.variance_reversion[j]) || !non_negative(d.variance_volatility[j]))
            return std::unexpected(std::format(
                "Segment {} has a reversion or volatility that is negative or not a number.",
                j + 1));
    if (!non_negative(d.skew_reversion) || !non_negative(d.skew_volatility) ||
        !non_negative(d.curvature_reversion) || !non_negative(d.curvature_volatility))
        return std::unexpected(std::string("The skew or curvature driver has a reversion or "
                                           "volatility that is negative or not a number."));

    const auto size = static_cast<Eigen::Index>(segments + 3);
    const auto& c = d.correlation;
    if (c.rows() != size || c.cols() != size)
        return std::unexpected(std::format(
            "The correlation is {}x{}; spot, {} segment(s), the skew and the curvature need {}x{}.",
            c.rows(),
            c.cols(),
            segments,
            size,
            size));
    if (!c.allFinite())
        return std::unexpected(std::string("The correlation holds an entry that is not a number."));
    for (Eigen::Index i = 0; i < size; ++i) {
        if (std::abs(c(i, i) - 1.0) > 1e-12)
            return std::unexpected(std::format("Correlation entry ({0},{0}) is not one.", i));
        for (Eigen::Index j = 0; j < i; ++j)
            if (std::abs(c(i, j) - c(j, i)) > 1e-12)
                return std::unexpected(
                    std::format("The correlation is not symmetric at ({},{}).", i, j));
    }
    return {};
}

}

std::expected<fx_vol_surface_generator, std::string> fx_vol_surface_generator::make(
    const ssvi_parameters& start, fx_vol_surface_dynamics dynamics, double dt) {
    if (!(dt > 0.0))
        return std::unexpected(std::string("A step must be a positive year fraction."));
    const auto& expiries = start.expiries;
    if (expiries.empty())
        return std::unexpected(std::string("The surface has no expiry."));
    if (!(std::abs(start.rho) < 1.0))
        return std::unexpected(std::format("The skew {} is not inside (-1, 1).", start.rho));
    if (!(start.eta > 0.0 && start.eta < curvature_bound))
        return std::unexpected(
            std::format("The curvature {} is not inside (0, sqrt(2)).", start.eta));

    fx_vol_surface_generator g;
    double years = 0.0;
    double theta = 0.0;
    for (const auto& e : expiries) {
        if (!(e.years > years))
            return std::unexpected(
                std::format("Expiry {} does not come after the one before it.", e.expiry_id));
        if (!(e.atm_total_variance > theta))
            return std::unexpected(
                std::format("The ATM total variance of {} does not rise from the expiry before it.",
                            e.expiry_id));
        const double segment = e.years - years;
        g.segment_years_.push_back(segment);
        g.variances_.push_back((e.atm_total_variance - theta) / segment);
        years = e.years;
        theta = e.atm_total_variance;
    }

    if (auto checked = check_dynamics(dynamics, expiries.size()); !checked)
        return std::unexpected(checked.error());
    const Eigen::LLT<Eigen::MatrixXd> root(dynamics.correlation);
    if (root.info() != Eigen::Success)
        return std::unexpected(std::string("The correlation is not positive definite."));

    g.root_ = root.matrixL();
    g.dynamics_ = std::move(dynamics);
    g.dt_ = dt;
    g.variance_levels_ = g.variances_;
    g.skew_driver_ = std::atanh(start.rho);
    g.skew_level_ = g.skew_driver_;
    g.curvature_driver_ = -std::log(curvature_bound / start.eta - 1.0);
    g.curvature_level_ = g.curvature_driver_;
    g.surface_.expiries = expiries;
    g.publish();
    return g;
}

std::size_t fx_vol_surface_generator::factor_count() const noexcept {
    return variances_.size() + 2;
}

const ssvi_parameters& fx_vol_surface_generator::step(double spot_shock,
                                                      std::span<const double> independent) {
    if (independent.size() != factor_count())
        throw std::invalid_argument(std::format(
            "A step reads {} independent normal(s), not {}.", factor_count(), independent.size()));
    Eigen::VectorXd e(static_cast<Eigen::Index>(factor_count() + 1));
    e(0) = spot_shock;
    for (std::size_t i = 0; i < independent.size(); ++i)
        e(static_cast<Eigen::Index>(i + 1)) = independent[i];
    // The root is lower triangular with a unit first entry, so the first shock is spot's own.
    const Eigen::VectorXd z = root_ * e;

    const double sqrt_dt = std::sqrt(dt_);
    for (std::size_t j = 0; j < variances_.size(); ++j) {
        const double v = variances_[j];
        const double level = variance_levels_[j];
        const double next = v + dynamics_.variance_reversion[j] * (level - v) * dt_ +
                            dynamics_.variance_volatility[j] * std::sqrt(v) * sqrt_dt *
                                z(static_cast<Eigen::Index>(j + 1));
        variances_[j] = std::max(next, level * variance_floor_fraction);
    }
    const auto skew_index = static_cast<Eigen::Index>(variances_.size() + 1);
    skew_driver_ = step_ou(skew_driver_,
                           skew_level_,
                           dynamics_.skew_reversion,
                           dynamics_.skew_volatility,
                           dt_,
                           z(skew_index));
    curvature_driver_ = step_ou(curvature_driver_,
                                curvature_level_,
                                dynamics_.curvature_reversion,
                                dynamics_.curvature_volatility,
                                dt_,
                                z(skew_index + 1));
    publish();
    return surface_;
}

const ssvi_parameters& fx_vol_surface_generator::step(double spot_shock, math::rng_stream& stream) {
    std::normal_distribution<double> normal;
    std::vector<double> independent(factor_count());
    for (auto& x : independent)
        x = normal(stream);
    return step(spot_shock, independent);
}

const ssvi_parameters& fx_vol_surface_generator::surface() const noexcept {
    return surface_;
}

void fx_vol_surface_generator::publish() {
    double theta = 0.0;
    for (std::size_t j = 0; j < variances_.size(); ++j) {
        theta += variances_[j] * segment_years_[j];
        surface_.expiries[j].atm_total_variance = theta;
    }
    surface_.rho = std::tanh(skew_driver_);
    surface_.eta = curvature_bound / (1.0 + std::exp(-curvature_driver_));
    surface_.gamma = fixed_gamma;
}

std::expected<std::vector<fx_vol_slice_quotes>, std::string>
fx_vol_surface_quotes(const ssvi_parameters& surface,
                      std::span<const fx_vol_slice_market> markets,
                      std::span<const double> deltas) {
    std::vector<fx_vol_slice_quotes> out;
    out.reserve(markets.size());
    std::vector<math::fx_wing_reading> wings(deltas.size());
    for (const auto& m : markets) {
        const auto expiry =
            std::ranges::find(surface.expiries, m.expiry_id, &ssvi_expiry_fit::expiry_id);
        if (expiry == surface.expiries.end())
            return std::unexpected(std::format("The surface has no expiry {}.", m.expiry_id));
        const double theta = expiry->atm_total_variance;
        const double years = expiry->years;
        const double phi = ssvi_phi(theta, surface.eta, surface.gamma);
        const auto smile = [&](double k) {
            return std::sqrt(ssvi_total_variance(k, theta, surface.rho, phi) / years);
        };
        const auto atm = math::read_smile(
            smile, std::sqrt(theta / years), years, m.market, m.convention, deltas, wings);
        if (!atm)
            return std::unexpected(std::format("Expiry {}: {}", m.expiry_id, atm.error()));
        fx_vol_slice_quotes q;
        q.expiry_id = m.expiry_id;
        q.atm = atm->volatility;
        for (const auto& w : wings) {
            q.risk_reversals.push_back(w.risk_reversal);
            q.butterflies.push_back(w.butterfly);
        }
        out.push_back(std::move(q));
    }
    return out;
}

std::optional<std::string> fx_vol_noise_profile_error(const fx_vol_noise_profile& p) {
    for (const auto& [name, value] : {std::pair{"missing", p.missing_probability},
                                      std::pair{"stale", p.stale_probability},
                                      std::pair{"outlier", p.outlier_probability}})
        if (!(value >= 0.0 && value <= 1.0))
            return std::format("The {} probability {} is not inside [0, 1].", name, value);
    for (const auto& [name, value] : {std::pair{"noise", p.noise},
                                      std::pair{"wing widening", p.wing_widening},
                                      std::pair{"expiry widening", p.expiry_widening},
                                      std::pair{"quote step", p.quote_step},
                                      std::pair{"outlier size", p.outlier_size}})
        if (!non_negative(value))
            return std::format("The {} {} is negative or not a number.", name, value);
    if (!std::isfinite(p.bias))
        return std::string("The bias is not a number.");
    return std::nullopt;
}

fx_vol_observation observe_fx_vol_point(double clean,
                                        double delta,
                                        double years,
                                        std::optional<double> previous,
                                        const fx_vol_noise_profile& profile,
                                        math::rng_stream& stream) {
    std::uniform_real_distribution<double> uniform(0.0, 1.0);
    // Every draw is taken before any outcome is read, and the shock is the inverse of one uniform
    // rather than a rejection sampler's, so each call reads the stream the same number of times.
    const double missing = uniform(stream);
    const double stale = uniform(stream);
    const double outlier = uniform(stream);
    const double outlier_side = uniform(stream);
    const double shock = math::normal_quantile(
        std::clamp(uniform(stream), std::numeric_limits<double>::min(), 1.0 - 1e-16));

    if (missing < profile.missing_probability)
        return {};
    if (previous && stale < profile.stale_probability)
        return {.value = previous, .stale = true};

    const double scale = profile.noise * (1.0 + profile.wing_widening * (0.5 - delta)) *
                         (1.0 + profile.expiry_widening * std::max(0.0, years - 1.0));
    double value = clean + profile.bias + scale * shock;
    if (outlier < profile.outlier_probability)
        value += outlier_side < 0.5 ? -profile.outlier_size : profile.outlier_size;
    if (profile.quote_step > 0.0)
        value = std::round(value / profile.quote_step) * profile.quote_step;
    return {.value = value};
}

}
