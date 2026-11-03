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

#include "ores.analytics.quant/math/gjr_garch.hpp"
#include "ores.marketdata.api/domain/feed_start.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
#include "ores.synthetic.api/domain/variance_calibration.hpp"
#include "ores.synthetic.api/export.hpp"
#include "ores.synthetic.api/feeds/fixing_calibration.hpp"
#include <chrono>
#include <expected>
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

/// What the approval rules judge a fit by, whether the fit is new or already stored.
struct fit_evidence final {
    int returns = 0;
    bool starts_agreed = false;
    double alpha = 0.0;
    double gamma = 0.0;
    double persistence = 0.0;
};

/// The evidence of a fitted shape; a rejected fit has none to judge.
ORES_SYNTHETIC_API_EXPORT fit_evidence evidence_of(const variance_fit& fit);

/**
 * @brief The rules @p evidence fails, each as a reader would say it; empty when the policy may
 * approve the fit.
 */
ORES_SYNTHETIC_API_EXPORT std::vector<std::string>
approval_failures(const fit_evidence& evidence, const variance_approval_rules& rules);

/// What a fit that passes the rules passed, for the reason the policy's approval records.
ORES_SYNTHETIC_API_EXPORT std::string approval_evidence(const fit_evidence& evidence,
                                                        const variance_approval_rules& rules);

/// A market day of the feed's clock, in hours: FX trades round the clock on business days.
inline constexpr double hours_per_market_day = 24.0;

/**
 * @brief E[z^2 1{z < 0}] of a market day's shock for a feed drawing @p components, per tick,
 * at @p ticks_per_hour: the share of a day's variance that falls on down days, which the
 * persistence of a GJR-GARCH shape depends on.
 */
ORES_SYNTHETIC_API_EXPORT double daily_p_minus(const std::vector<mixture_component>& components,
                                               int ticks_per_hour);

/// The shape a feed runs, and the record of it its start keeps.
struct feed_variance final {
    ores::analytics::quant::math::gjr_garch_shape shape;
    ores::marketdata::domain::variance_start record;
};

/**
 * @brief The GJR-GARCH shape a feed runs: the one of the approved calibration its config names.
 *
 * A feed fits nothing at start, so a calibration that is missing, of another pair, not approved
 * or without a shape refuses the start, with the reason a person reads, rather than running a
 * variance nobody reviewed for this pair. The record names the calibration and its version, and
 * states the persistence under the feed's own mixture.
 */
ORES_SYNTHETIC_API_EXPORT std::expected<feed_variance, std::string>
calibrated_variance(const ores::synthetic::domain::fx_spot_generation_config& cfg,
                    const std::optional<ores::synthetic::domain::variance_calibration>& calibration,
                    const std::vector<ores::synthetic::domain::gmm_component>& components);

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
