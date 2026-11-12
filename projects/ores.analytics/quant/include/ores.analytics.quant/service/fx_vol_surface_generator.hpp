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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_FX_VOL_SURFACE_GENERATOR_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_FX_VOL_SURFACE_GENERATOR_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/fx_delta.hpp"
#include "ores.analytics.quant/math/fx_smile_quotes.hpp"
#include "ores.analytics.quant/math/rng_stream.hpp"
#include "ores.analytics.quant/service/ssvi_calibration_engine.hpp"
#include <Eigen/Cholesky>
#include <Eigen/Core>
#include <cstddef>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief How the state of a synthetic FX volatility surface moves.
 *
 * The state is the forward variance of each segment between quoted expiries, each a
 * Cox-Ingersoll-Ross process, and two Ornstein-Uhlenbeck drivers: the skew is the hyperbolic
 * tangent of the first, the curvature a bounded logistic of the second. Every process reverts to
 * where the surface started. See the option vol surfaces section of the synthetic market data
 * generation approach.
 */
struct fx_vol_surface_dynamics final {
    /// One for each segment, in expiry order: the reversion speed of its forward variance.
    std::vector<double> variance_reversion;
    /// One for each segment: the volatility of its forward variance.
    std::vector<double> variance_volatility;
    double skew_reversion = 0.0;
    double skew_volatility = 0.0;
    double curvature_reversion = 0.0;
    double curvature_volatility = 0.0;
    /**
     * @brief The correlation of the shocks: spot first, then each segment's forward variance in
     * expiry order, then the skew driver, then the curvature driver.
     */
    Eigen::MatrixXd correlation;
};

/**
 * @brief A synthetic FX volatility surface: an SSVI surface whose parameters evolve.
 *
 * The exponent gamma is fixed at 1/2, so the curvature is phi(theta) = eta / sqrt(theta) and
 * every surface it emits is free of static arbitrage while each ATM total variance theta stays
 * below two. The forward variances are positive, so theta rises with the expiry; the skew lies in
 * (-1, 1) and the curvature eta in (0, sqrt(2)). The butterfly condition
 * theta * phi * (1 + |rho|) < 4 reads eta * sqrt(theta) * (1 + |rho|) < 4, which these bounds
 * give only while theta < 2.
 *
 * The forward variances step by Euler with a floor, because their shocks must be correlated with
 * spot's and the exact transition draws its own. The Feller condition is not enforced, and a long
 * step biases the variance paths toward the floor.
 */
class ORES_ANALYTICS_QUANT_EXPORT fx_vol_surface_generator final {
public:
    /**
     * @brief Starts at a fitted surface, which is also the level every process reverts to.
     *
     * The start's ATM total variances, skew and curvature are kept, and its gamma is ignored.
     * Refuses a start whose ATM total variance does not rise strictly with the expiry, a skew or
     * a curvature outside its bounds, dynamics that do not name one value for each segment, a
     * correlation that is not a positive definite matrix of the right size with a unit diagonal,
     * a reversion, volatility or correlation that is not a finite number, and a step that is not
     * positive.
     *
     * @param dt The year fraction one step represents.
     */
    [[nodiscard]] static std::expected<fx_vol_surface_generator, std::string>
    make(const ssvi_parameters& start, fx_vol_surface_dynamics dynamics, double dt);

    /// The number of independent standard normals each step reads besides spot's.
    [[nodiscard]] std::size_t factor_count() const noexcept;

    /**
     * @brief Advances the state one step.
     *
     * @param spot_shock The standard normal spot's own step drew, so the surface moves with
     * spot by the correlation.
     * @param independent One independent standard normal for each factor; see factor_count.
     * @throws std::invalid_argument when independent does not hold factor_count normals.
     */
    const ssvi_parameters& step(double spot_shock, std::span<const double> independent);

    /// Advances the state one step, drawing its independent normals from the stream.
    const ssvi_parameters& step(double spot_shock, math::rng_stream& stream);

    /// The surface the state gives now.
    [[nodiscard]] const ssvi_parameters& surface() const noexcept;

private:
    fx_vol_surface_generator() = default;
    void publish();

    fx_vol_surface_dynamics dynamics_;
    Eigen::MatrixXd root_;
    double dt_ = 0.0;
    std::vector<double> segment_years_;
    std::vector<double> variances_;
    std::vector<double> variance_levels_;
    double skew_driver_ = 0.0;
    double skew_level_ = 0.0;
    double curvature_driver_ = 0.0;
    double curvature_level_ = 0.0;
    ssvi_parameters surface_;
};

/// Where one expiry of a surface is quoted: its market and the pair's convention.
struct fx_vol_slice_market final {
    std::string expiry_id;
    math::fx_market market{};
    math::fx_quote_convention convention{};
};

/// The quotes of one expiry, as a broker quotes the pair: ATM, then a risk reversal and a
/// butterfly at each delta.
struct fx_vol_slice_quotes final {
    std::string expiry_id;
    double atm = 0.0;
    std::vector<double> risk_reversals;
    std::vector<double> butterflies;
};

/**
 * @brief The clean quotes of a surface, at the deltas given, in each expiry's convention.
 *
 * Refuses a market that names an expiry the surface does not hold, and a slice the convention
 * cannot read.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT
    std::expected<std::vector<fx_vol_slice_quotes>, std::string>
    fx_vol_surface_quotes(const ssvi_parameters& surface,
                          std::span<const fx_vol_slice_market> markets,
                          std::span<const double> deltas);

/**
 * @brief How a synthetic contributor's quotes differ from the clean ones.
 *
 * A profile with every field zero is the clean contributor, whose quotes are the truth.
 */
struct fx_vol_noise_profile final {
    /// The standard deviation of the measurement noise at an ATM point of one year, in volatility.
    double noise = 0.0;
    /// How much larger the noise is on a wing: a factor of (1 + wing_widening * (0.5 - delta)).
    double wing_widening = 0.0;
    /// How much larger the noise is for each year past one.
    double expiry_widening = 0.0;
    /// The step the contributor rounds its quotes to; zero for none.
    double quote_step = 0.0;
    /// The chance a point is not published this step.
    double missing_probability = 0.0;
    /// The chance a point repeats the contributor's last value for it.
    double stale_probability = 0.0;
    /// The chance a point is off by outlier_size, in either direction.
    double outlier_probability = 0.0;
    double outlier_size = 0.0;
    /// A persistent offset of every point, in volatility.
    double bias = 0.0;
};

/**
 * @brief Why a noise profile cannot be applied, or nothing when it can.
 *
 * Refuses a probability outside [0, 1] and a negative noise, widening, quote step or outlier size.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT std::optional<std::string>
fx_vol_noise_profile_error(const fx_vol_noise_profile& profile);

/// What a contributor publishes for one point.
struct fx_vol_observation final {
    /// Empty when the contributor publishes nothing for the point this step.
    std::optional<double> value;
    bool stale = false;
};

/**
 * @brief One contributor's observation of one clean point.
 *
 * Each call draws the same number of values from the stream, whatever the profile and the
 * outcome, so a change to one field of a profile does not move a later point's draws. A noisy
 * value is not clamped: a contributor can publish a volatility a desk would discard.
 *
 * @param delta The point's delta, in (0, 0.5]; 0.5 for ATM.
 * @param years The point's expiry.
 * @param previous The value the contributor last published for the point, which a stale point
 * repeats unchanged.
 * @param profile A profile fx_vol_noise_profile_error accepts.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT fx_vol_observation
observe_fx_vol_point(double clean,
                     double delta,
                     double years,
                     std::optional<double> previous,
                     const fx_vol_noise_profile& profile,
                     math::rng_stream& stream);

}

#endif
