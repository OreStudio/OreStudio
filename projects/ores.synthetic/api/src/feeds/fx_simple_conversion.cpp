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
#include "ores.synthetic.api/feeds/fx_simple_conversion.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>

namespace ores::synthetic::feed {

namespace {

constexpr double jump_volatility_ratio = 4.0;
constexpr double jump_detection_ratio = 3.0;

bool is_ornstein_uhlenbeck(const std::string& process_type) {
    if (process_type == "ornstein_uhlenbeck")
        return true;
    if (process_type == "geometric" || process_type == "arithmetic")
        return false;
    throw std::invalid_argument("Unknown process type: " + process_type);
}

void require_positive(double price, int ticks_per_hour) {
    if (!(price > 0.0))
        throw std::invalid_argument("The initial price must be positive.");
    if (ticks_per_hour <= 0)
        throw std::invalid_argument("The ticks per hour must be positive.");
}

/// Within the rounding of the three significant digits the seeded mixtures keep.
bool close(double a, double b) {
    return std::abs(a - b) <= 1e-2 * std::max(std::abs(a), std::abs(b));
}

bool negligible(double value, double against) {
    return std::abs(value) <= 1e-6 * std::abs(against);
}

}

fx_mixture fx_mixture_from_simple(const fx_simple_view& view,
                                  const std::string& process_type,
                                  double initial_price,
                                  int ticks_per_hour) {
    const bool ou = is_ornstein_uhlenbeck(process_type);
    require_positive(initial_price, ticks_per_hour);
    if (view.volatility_pct_per_hour < 0.0)
        throw std::invalid_argument("The volatility cannot be negative.");

    const double updates = ticks_per_hour;
    const double scale = process_type == "geometric" ? 1.0 : initial_price;
    const double stdev = view.volatility_pct_per_hour / 100.0 / std::sqrt(updates) * scale;

    if (ou)
        return {.means = {0.0},
                .stdevs = {stdev},
                .weights = {kappa_for_half_life(view.half_life_minutes, ticks_per_hour)}};

    if (view.jump_share_pct < 0.0 || view.jump_share_pct > 100.0)
        throw std::invalid_argument("The jump share must be between 0 and 100.");
    const double share = view.jump_share_pct / 100.0;
    const double mean = view.trend_pct_per_hour / 100.0 / updates * scale;

    fx_mixture out{.means = {mean}, .stdevs = {stdev}, .weights = {1.0 - share}};
    if (share > 0.0) {
        out.means.push_back(0.0);
        out.stdevs.push_back(stdev * jump_volatility_ratio);
        out.weights.push_back(share);
    }
    return out;
}

fx_simple_reading fx_simple_from_mixture(const fx_mixture& mixture,
                                         const std::string& process_type,
                                         double initial_price,
                                         int ticks_per_hour) {
    const bool ou = is_ornstein_uhlenbeck(process_type);
    require_positive(initial_price, ticks_per_hour);
    if (mixture.means.size() != mixture.stdevs.size() ||
        mixture.means.size() != mixture.weights.size())
        throw std::invalid_argument("The means, stdevs and weights differ in size.");

    fx_simple_reading out;
    if (mixture.means.empty()) {
        out.reason = "The feed has no components.";
        return out;
    }

    const double updates = ticks_per_hour;
    const double scale = process_type == "geometric" ? 1.0 : initial_price;
    const auto per_hour_pct = [&](double per_update, bool spread_by_root) {
        return per_update / scale * 100.0 * (spread_by_root ? std::sqrt(updates) : updates);
    };

    std::size_t primary = 0;
    for (std::size_t i = 1; i < mixture.weights.size(); ++i)
        if (mixture.weights[i] > mixture.weights[primary])
            primary = i;
    out.view.volatility_pct_per_hour = per_hour_pct(mixture.stdevs[primary], true);

    if (ou) {
        const double kappa = mixture.weights.front();
        out.view.volatility_pct_per_hour = per_hour_pct(mixture.stdevs.front(), true);
        if (kappa > 0.0)
            out.view.half_life_minutes = std::log(2.0) / kappa * 60.0 / updates;
        out.exact = mixture.means.size() == 1 &&
                    negligible(mixture.means.front(), mixture.stdevs.front()) && kappa > 0.0;
        if (!out.exact)
            out.reason = kappa > 0.0 ? "The engine reads one component, and this has more." :
                                       "The price never returns to its long-run level.";
        return out;
    }

    out.view.trend_pct_per_hour = per_hour_pct(mixture.means[primary], false);
    double total = 0.0;
    double jump = 0.0;
    std::size_t jumps = 0;
    bool jump_shaped = true;
    for (std::size_t i = 0; i < mixture.weights.size(); ++i) {
        total += mixture.weights[i];
        if (i == primary)
            continue;
        if (mixture.stdevs[i] >= jump_detection_ratio * mixture.stdevs[primary]) {
            jump += mixture.weights[i];
            ++jumps;
            jump_shaped = jump_shaped && negligible(mixture.means[i], mixture.stdevs[i]) &&
                          close(mixture.stdevs[i], jump_volatility_ratio * mixture.stdevs[primary]);
        } else {
            jump_shaped = false;
        }
    }
    if (total > 0.0)
        out.view.jump_share_pct = jump / total * 100.0;
    out.exact = total > 0.0 && jump_shaped && jumps == mixture.weights.size() - 1 && jumps <= 1;
    if (!out.exact)
        out.reason = "The components are set by hand.";
    return out;
}

}
