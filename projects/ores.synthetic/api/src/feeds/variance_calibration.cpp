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
#include "ores.synthetic.api/feeds/variance_calibration.hpp"
#include "ores.analytics.quant/math/gjr_garch.hpp"
#include "ores.synthetic.api/feeds/fixing_calibration.hpp"
#include <charconv>
#include <cmath>
#include <cstddef>
#include <numbers>
#include <numeric>
#include <string>

namespace ores::synthetic::feed {

namespace {

std::optional<double> number_in(const std::string& text) {
    double value = 0.0;
    const auto* const end = text.data() + text.size();
    const auto [stop, ec] = std::from_chars(text.data(), end, value);
    if (ec != std::errc{} || stop != end || !std::isfinite(value))
        return std::nullopt;
    return value;
}

}

std::vector<variance_point>
capture_variance_points(const std::vector<std::chrono::year_month_day>& dates,
                        const std::vector<std::string>& texts,
                        bool inverted) {
    const auto n = std::min(dates.size(), texts.size());
    std::vector<variance_point> out;
    out.reserve(n);
    std::optional<double> previous;
    for (std::size_t i = 0; i < n; ++i) {
        variance_point p{.date = dates[i], .text = texts[i]};
        p.value = number_in(texts[i]);
        if (!p.value) {
            p.exclusion_reason = "the value is not a number";
        } else if (*p.value <= 0.0) {
            p.exclusion_reason = "the value is not positive";
        } else if (!previous) {
            p.exclusion_reason = "the first readable point has no return";
            previous = p.value;
        } else {
            const double r = std::log(*p.value / *previous);
            p.log_return = inverted ? -r : r;
            p.used = true;
            previous = p.value;
        }
        out.push_back(std::move(p));
    }
    return out;
}

double daily_p_minus(const std::vector<mixture_component>& components, int ticks_per_hour) {
    namespace qm = ores::analytics::quant::math;
    qm::mixture per_tick;
    double total = 0.0;
    for (const auto& c : components) {
        per_tick.means.push_back(c.mean);
        per_tick.stdevs.push_back(c.stdev);
        per_tick.weights.push_back(c.weight);
        total += c.weight;
    }
    if (total > 0.0)
        for (auto& w : per_tick.weights)
            w /= total;
    return qm::daily_negative_second_moment(per_tick, ticks_per_hour * hours_per_market_day);
}

variance_fit fit_variance(const std::vector<variance_point>& points, double p_minus) {
    namespace qm = ores::analytics::quant::math;
    std::vector<double> returns;
    returns.reserve(points.size());
    for (const auto& p : points)
        if (p.used && p.log_return)
            returns.push_back(*p.log_return);

    variance_fit out;
    out.returns_used = static_cast<int>(returns.size());
    if (out.returns_used < minimum_garch_returns) {
        out.reason = "the input holds " + std::to_string(out.returns_used) +
                     " daily returns and a fit needs " + std::to_string(minimum_garch_returns);
        return out;
    }
    const auto fit = qm::fit_gjr_garch(returns, p_minus);
    if (!fit) {
        out.reason = "the fit failed: " + fit.error();
        return out;
    }
    const double mean =
        std::accumulate(returns.begin(), returns.end(), 0.0) / static_cast<double>(returns.size());
    std::vector<double> shocks;
    shocks.reserve(returns.size());
    for (const double r : returns)
        shocks.push_back(r - mean);
    const double long_run = fit->model.long_run_variance();
    const auto variances = qm::filter(fit->model, shocks, long_run);
    const auto shape = fit->model.shape();

    out.fitted = true;
    out.alpha = shape.alpha;
    out.gamma = shape.gamma;
    out.beta = shape.beta;
    out.persistence = fit->model.persistence();
    out.long_run_variance = long_run;
    out.starting_variance_ratio = variances.back() / long_run;
    out.log_likelihood =
        -static_cast<double>(returns.size()) * (fit->cost + 0.5 * std::log(2.0 * std::numbers::pi));
    out.starts = fit->starts_tried;
    out.starts_agreed = fit->starts_agreed;
    return out;
}

}
