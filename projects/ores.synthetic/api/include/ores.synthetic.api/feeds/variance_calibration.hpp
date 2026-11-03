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
#ifndef ORES_SYNTHETIC_API_FEEDS_VARIANCE_CALIBRATION_HPP
#define ORES_SYNTHETIC_API_FEEDS_VARIANCE_CALIBRATION_HPP

#include "ores.synthetic.api/export.hpp"
#include "ores.synthetic.api/feeds/fixing_calibration.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::feed {

/// One fixing date of a captured window, as a variance calibration input stores it.
struct variance_point final {
    std::chrono::year_month_day date;
    /// The fixing as the store held it.
    std::string text;
    /// The fixing read as a number in the item's own convention, when it is one.
    std::optional<double> value;
    /// The daily log return to this point in the pair's market convention.
    std::optional<double> log_return;
    bool used = false;
    /// Why the point is not used; empty when it is.
    std::string exclusion_reason;
};

/**
 * @brief The points of a window of fixings, oldest first.
 *
 * A fixing that is not a number, or not positive, is kept with its reason. The return to a
 * point is the log of its value over the previous readable value, negated when the item is
 * @p inverted, so every return is in the pair's market convention; the first readable point
 * has no return. Every date of the window is a point, so the input states what the fit did
 * not use as well as what it did.
 */
ORES_SYNTHETIC_API_EXPORT std::vector<variance_point>
capture_variance_points(const std::vector<std::chrono::year_month_day>& dates,
                        const std::vector<std::string>& texts,
                        bool inverted);

/// What a GJR-GARCH fit made of a set of points: a proposed shape, or the reason it has none.
struct variance_fit final {
    /// True when the shape was fitted; false when the attempt is rejected.
    bool fitted = false;
    /// Why the attempt is rejected; empty when fitted.
    std::string reason;
    std::optional<double> alpha;
    std::optional<double> gamma;
    std::optional<double> beta;
    std::optional<double> persistence;
    std::optional<double> long_run_variance;
    std::optional<double> starting_variance_ratio;
    /// The Gaussian log likelihood of the returns at the fitted shape.
    std::optional<double> log_likelihood;
    int returns_used = 0;
    int starts = 0;
    bool starts_agreed = false;
};

/// A market day of the feed's clock, in hours: FX trades round the clock on business days.
inline constexpr double hours_per_market_day = 24.0;

/**
 * @brief E[z^2 1{z < 0}] of a market day's shock for a feed drawing @p components, per tick,
 * at @p ticks_per_hour: the share of a day's variance that falls on down days, which the
 * persistence of a GJR-GARCH shape depends on.
 */
ORES_SYNTHETIC_API_EXPORT double daily_p_minus(const std::vector<mixture_component>& components,
                                               int ticks_per_hour);

/// The version of the fit below, stored with every calibration it makes.
inline constexpr int variance_fit_method_version = 1;

/**
 * @brief Fit a GJR-GARCH shape to the used returns of @p points by variance targeting and
 * Gaussian quasi maximum likelihood.
 *
 * Reads only the points, so the same input always gives the same fit. Fewer than
 * minimum_garch_returns returns, or a fit that reaches no admissible shape, is a rejection
 * with its reason. A fitted shape states its persistence under @p p_minus, the variance at
 * the end of the history over its long-run level, the likelihood, and whether every start of
 * the simplex reached the same optimum.
 *
 * @param p_minus The feed's own mixture's daily E[z^2 1{z < 0}].
 */
ORES_SYNTHETIC_API_EXPORT variance_fit fit_variance(const std::vector<variance_point>& points,
                                                    double p_minus);

}

#endif
