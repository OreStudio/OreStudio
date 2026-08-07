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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_CURVE_HEALTH_CHECKER_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_CURVE_HEALTH_CHECKER_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/service/curve_bootstrap_engine.hpp"
#include "ores.analytics.quant/service/forward_rate_calculator.hpp"
#include <string>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief How seriously a curve_health_finding should be treated -- deliberately not a bool
 * pass/fail, since not every finding here is actually wrong (a negative forward rate is
 * financially plausible in a sustained negative-rate regime; a non-positive discount factor is
 * not).
 */
enum class curve_health_severity { INFO, WARNING };

/**
 * @brief One observation about a bootstrapped curve's shape, from curve_health_checker.
 */
struct ORES_ANALYTICS_QUANT_EXPORT curve_health_finding {
    curve_health_severity severity = curve_health_severity::INFO;
    /// The point/pair this finding is about, echoing bootstrapped_point::point_id or
    /// forward_rate_point::point_id.
    std::string point_id;
    std::string message;
};

/**
 * @brief Surfaces a bootstrapped curve's shape problems as findings a UI can render as a
 * warning banner -- the building block behind the Curve Builder Workbench's forward-curve
 * "health check" plot, but not tied to that one caller (any consumer of a bootstrapped curve
 * can ask "does this look right").
 *
 * Deliberately narrow: this is not an options-pricing arbitrage checker (there is no
 * "negative variance" concept for a deterministic discount curve -- that is a vol-surface/
 * Dupire-local-vol concern, a different domain entirely). What it checks:
 * - A non-positive discount factor is always a genuine bug (df = exp(-integral of rate) is
 *   mathematically required to stay strictly positive) -- reported regardless of rate regime.
 * - A negative forward rate is financially plausible (sustained negative-rate regimes are real,
 *   e.g. EUR/JPY/CHF historically) -- reported as INFO, not WARNING, never treated as wrong by
 *   itself.
 * - A forward rate that is a statistical outlier against the rest of the series (a leave-one-out
 *   z-score beyond @p outlier_z_threshold standard deviations, deliberately excluding the
 *   candidate itself from its own mean/stdev -- see check_forward_rates()'s own doc for why) is
 *   the actual "flawed composition" signal -- a bad interpolation choice, a fat-fingered pillar
 *   quote, or overlapping tenors -- reported as WARNING.
 * Non-monotonic discount factors and discount factors above 1.0 are deliberately *not* flagged:
 * both are legitimate under a negative-rate regime, not proof of a bootstrap error.
 */
class ORES_ANALYTICS_QUANT_EXPORT curve_health_checker {
public:
    /**
     * @brief Flags any non-positive discount factor in @p points.
     */
    static std::vector<curve_health_finding>
    check_discount_factors(const std::vector<bootstrapped_point>& points);

    /**
     * @brief Flags negative (INFO) and statistically-outlying (WARNING) forward rates in
     * @p forwards.
     *
     * @param outlier_z_threshold How many standard deviations from the rest of the series
     * (leave-one-out) a forward rate must deviate by to be flagged as a discontinuity. Fewer than 3
     * points in @p forwards yields no outlier findings -- a sample that small has no meaningful
     * notion of "outlier".
     */
    static std::vector<curve_health_finding>
    check_forward_rates(const std::vector<forward_rate_point>& forwards,
                        double outlier_z_threshold = 3.0);
};

}

#endif
