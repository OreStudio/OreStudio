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
#include "ores.analytics.quant/service/curve_health_checker.hpp"
#include <cmath>
#include <format>
#include <numeric>
#include <utility>

namespace ores::analytics::quant::service {

namespace {

// Mean/stdev of every rate in @p forwards except the one at @p excluded_index -- deliberately
// leave-one-out rather than population stats over the whole series including the candidate
// itself: a z-score computed with the outlier folded into its own mean/stdev is self-limiting
// (for n points, |z| is bounded near sqrt(n-1) no matter how extreme the outlier is), which
// would silently mask exactly the small-curve case (5-15 pillars) this check most needs to
// catch.
std::pair<double, double> leave_one_out_mean_stdev(const std::vector<forward_rate_point>& forwards,
                                                   std::size_t excluded_index) {
    double sum = 0.0;
    std::size_t n = 0;
    for (std::size_t i = 0; i < forwards.size(); ++i) {
        if (i == excluded_index)
            continue;
        sum += forwards[i].instantaneous_forward_rate;
        ++n;
    }
    const double mean = sum / static_cast<double>(n);

    double sum_sq = 0.0;
    for (std::size_t i = 0; i < forwards.size(); ++i) {
        if (i == excluded_index)
            continue;
        const double d = forwards[i].instantaneous_forward_rate - mean;
        sum_sq += d * d;
    }
    return {mean, std::sqrt(sum_sq / static_cast<double>(n))};
}

}

std::vector<curve_health_finding>
curve_health_checker::check_discount_factors(const std::vector<bootstrapped_point>& points) {
    std::vector<curve_health_finding> findings;
    for (const auto& p : points) {
        if (p.discount_factor <= 0.0) {
            findings.push_back({curve_health_severity::WARNING,
                                p.point_id,
                                std::format("Non-positive discount factor ({:.6g}) -- the "
                                            "bootstrap solve for this pillar is broken.",
                                            p.discount_factor)});
        }
    }
    return findings;
}

std::vector<curve_health_finding>
curve_health_checker::check_forward_rates(const std::vector<forward_rate_point>& forwards,
                                          double outlier_z_threshold) {
    std::vector<curve_health_finding> findings;

    for (const auto& f : forwards) {
        if (f.instantaneous_forward_rate < 0.0) {
            findings.push_back(
                {curve_health_severity::INFO,
                 f.point_id,
                 std::format("Negative forward rate ({:.4f}%) -- plausible under a sustained "
                             "negative-rate regime, not necessarily a bootstrap error.",
                             f.instantaneous_forward_rate * 100.0)});
        }
    }

    if (forwards.size() < 3)
        return findings;

    for (std::size_t i = 0; i < forwards.size(); ++i) {
        const auto [mean, stdev] = leave_one_out_mean_stdev(forwards, i);
        if (stdev == 0.0)
            continue;

        const double z = (forwards[i].instantaneous_forward_rate - mean) / stdev;
        if (std::abs(z) > outlier_z_threshold) {
            findings.push_back(
                {curve_health_severity::WARNING,
                 forwards[i].point_id,
                 std::format("Forward rate ({:.4f}%) is a statistical outlier ({:.1f} standard "
                             "deviations from the rest of the series) -- check for a bad pillar "
                             "quote, overlapping tenors, or an interpolation-method mismatch.",
                             forwards[i].instantaneous_forward_rate * 100.0,
                             z)});
        }
    }

    return findings;
}

}
