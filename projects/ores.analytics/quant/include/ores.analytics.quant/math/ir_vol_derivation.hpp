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
#ifndef ORES_ANALYTICS_QUANT_MATH_IR_VOL_DERIVATION_HPP
#define ORES_ANALYTICS_QUANT_MATH_IR_VOL_DERIVATION_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/return_moments.hpp"
#include "ores.analytics.quant/math/volatility_measurement.hpp"
#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::analytics::quant::math {

/**
 * @file ir_vol_derivation.hpp
 * @brief The interest rate volatility a rate history implies.
 *
 * The measurement engine says how volatile a rate's history is. It does not say what an option on
 * that rate is worth, because a swaption's expiry and the tenor of its swap are not in the history:
 * they are the model's to state. This file carries that statement, and it is deliberately small.
 *
 * Two facts about the object decide the arithmetic.
 *
 * - A swaption's ATM volatility is a short rate's volatility carried over the option's life and
 *   averaged over the swap's tenor. In a one-factor Gaussian short-rate model both effects are
 *   closed form, so the projection is a product of two factors and no valuation is needed.
 * - The strike axis is the shape of the returns, not their size. The first-order Gram-Charlier
 *   correction says the volatility at a strike moves with the skewness and the standardised
 * distance of the strike from the money: the smile is a straight line in the standardised strike.
 *
 * Neither step names an asset class, a currency or a tenor. The inputs are a level, a shape and a
 * pillar expressed in years, and the caller states the coordinates the quotes are named by.
 */

/**
 * @brief A one-factor Gaussian short-rate model, as the derivation reads it from a curve.
 *
 * The projection needs one parameter of the model, its mean reversion, and the volatility it runs
 * at. The volatility is not a parameter here: it is what the measurement read, because the model's
 * level and the history's level are the same number stated twice.
 */
enum class short_rate_model : std::uint8_t {
    /// dr = a (theta - r) dt + sigma dW, with theta held at one value.
    vasicek,
    /// The same process with a time-dependent theta. The projection is the same function.
    hull_white,
    /// Named and refused: a state-dependent volatility is not this projection's closed form.
    cox_ingersoll_ross,
    /// Named and refused: a log-normal short rate is not this projection's closed form.
    black_karasinski,
    /// Named and refused: two factors where the projection reads one.
    two_factor_gaussian
};

/// Whether the projection is closed form for @p model.
ORES_ANALYTICS_QUANT_EXPORT bool has_closed_form(short_rate_model model) noexcept;

/// The model's name, as a stored series states it.
ORES_ANALYTICS_QUANT_EXPORT std::string_view name_of(short_rate_model model);

/**
 * @brief The model a name states, or nothing for a process the derivation cannot project through.
 *
 * A cox-ingersoll-ross short rate has a state-dependent volatility, a two-factor Gaussian model has
 * two factors, and a Black-Karasinski model is log-normal: none of them is the process this
 * projection is closed form for, and each is refused by name rather than approximated by the wrong
 * formula.
 */
ORES_ANALYTICS_QUANT_EXPORT std::optional<short_rate_model>
short_rate_model_named(std::string_view name);

/// The model and the one parameter the projection reads from it.
struct short_rate_projection final {
    short_rate_model model{short_rate_model::hull_white};
    /// Mean reversion, in inverse years. Zero is the driftless limit.
    double mean_reversion = 0.0;
};

/**
 * @brief The smile across strikes, from the same returns' shape.
 *
 * The first-order Gram-Charlier correction to a normal density gives an implied volatility that
 * moves linearly in the standardised distance of the strike from the money, with the skewness as
 * its slope over six. The model therefore states two numbers: the level at the money, and the
 * skewness the shape measured. It says nothing about the curvature, which a smile needs and which
 * this model does not fit; a class whose market is convex in the strike needs a model that fits it.
 */
struct normal_skew_smile final {
    /// The level at the money, in the series' unit a year.
    double alpha = 0.0;
    /// The skewness of the returns, which sets the slope.
    double skewness = 0.0;
};

/// The smile a level and a shape state. @p shape is the shape of a return over the pillar's own
/// length, which the caller scales from the daily shape with over_days.
ORES_ANALYTICS_QUANT_EXPORT normal_skew_smile smile_of(double atm_volatility,
                                                       return_shape shape) noexcept;

/**
 * @brief The volatility the smile states at a strike.
 *
 * The strike enters standardised: the distance from the forward over the level times the root of
 * the expiry. A strike on the money is the level itself, a negative skewness raises the volatility
 * of the strikes below the money, and the result is held above zero so a far wing cannot state a
 * negative volatility.
 */
ORES_ANALYTICS_QUANT_EXPORT double smile_at(const normal_skew_smile& smile,
                                            double forward,
                                            double strike,
                                            double expiry_years) noexcept;

/// One swaption pillar: the option's expiry, and the tenor of the swap it is written on.
struct swaption_pillar final {
    double expiry_years;
    double term_years;

    friend bool operator==(const swaption_pillar&, const swaption_pillar&) = default;
};

/**
 * @brief The factor a short rate's level is carried by, from today to a swaption's ATM.
 *
 * The factor is the bond-option approximation, not an exact swaption volatility. Its first term is
 * the variance of the short rate at the option's expiry, (1 - exp(-2 a T)) / (2 a), rooted; its
 * second is the damping a swap's tenor imposes by averaging the short rate over the swap's own
 * length, (1 - exp(-a tau)) / (a tau). The product is the projection.
 *
 * As a tends to zero the first term tends to sqrt(T) and the second to one, so a curve that runs no
 * mean reversion is read through the limit and not refused. That limit is the short rate's spread,
 * not the spread of the rate averaged over the tenor, whose driftless value would be
 * sqrt(T + tau / 3): the approximation understates a long tenor where mean reversion is small, and
 * a model fitted with no reversion should be read with that in mind.
 *
 * Refused for a pillar that is not positive, because neither factor means anything without a
 * length, and for a negative mean reversion: the expression is continuous through it, but a rate
 * that diverges from its target is not a model this platform projects through.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<double, std::string>
projection_factor(swaption_pillar pillar, short_rate_projection model);

/// What one pillar's ATM volatility came from.
struct ir_pillar_vol final {
    /// The returns the term's level was read over.
    int window;
    /// The term's measured level, in the series' unit a year.
    double level;
    /// The model's dimensionless factor for the pillar.
    double factor;
    /// The ATM normal volatility: the level carried by the factor.
    double atm;
    /// The smile the shape of a return over the pillar's own life states.
    normal_skew_smile smile;
};

/**
 * @brief One pillar's ATM volatility and smile, from the window's level, the model and a shape.
 *
 * @p horizon_shape is the shape of a return over the pillar's own life, which the caller scales
 * from the daily shape with over_days. A daily shape read at a one-year pillar would inflate the
 * smile's slope by the root of the periods in a year.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<ir_pillar_vol, std::string>
atm_vol_of(double level,
           int window,
           swaption_pillar pillar,
           short_rate_projection model,
           return_shape horizon_shape);

/// One term's matrix: the shape read once, and one cell per expiry of the ladder.
struct ir_term_matrix final {
    series_measurement series;
    /// One cell per expiry, in the order asked for: the pillar's volatility or why it has none.
    std::vector<std::expected<ir_pillar_vol, std::string>> cells;
};

/**
 * @brief One term's ATM matrix across an expiry ladder, from the term's own returns.
 *
 * The series is a term's levels, and this function applies the class's transform: a difference of
 * the levels, because a rate may cross zero. The level of each cell is then read by the measurement
 * engine over the window its expiry states, the shape is read once over every return held, and the
 * smile is read from the shape of a return over that cell's own life. The model carries the level
 * to the swaption.
 *
 * The levels are in whatever unit the caller keeps them, so the matrix is in that unit a year: the
 * store's basis points are the caller's to state, and the engine does not convert. A cell the
 * history does not cover is refused alone, with the returns its window needs and the number the
 * sample holds.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<ir_term_matrix, std::string>
derive_term_matrix(std::span<const dated_level> levels,
                   double term_years,
                   std::span<const double> expiry_years,
                   const measurement_settings& settings,
                   short_rate_projection model);

}

#endif
