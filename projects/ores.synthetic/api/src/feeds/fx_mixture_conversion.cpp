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
#include "ores.synthetic.api/feeds/fx_mixture_conversion.hpp"
#include <cmath>
#include <stdexcept>

namespace ores::synthetic::feed {

namespace {

const std::string geometric = "geometric";
const std::string arithmetic = "arithmetic";
const std::string ornstein_uhlenbeck = "ornstein_uhlenbeck";

void require_known(const std::string& process_type) {
    if (process_type != geometric && process_type != arithmetic &&
        process_type != ornstein_uhlenbeck)
        throw std::invalid_argument("Unknown process type: " + process_type);
}

}

double kappa_for_half_life(double minutes, int ticks_per_hour) {
    return std::log(2.0) / (minutes * ticks_per_hour / 60.0);
}

fx_mixture convert_fx_mixture(const fx_mixture& mixture,
                              const std::string& from_process_type,
                              const std::string& to_process_type,
                              double initial_price,
                              int ticks_per_hour) {
    require_known(from_process_type);
    require_known(to_process_type);
    if (mixture.means.size() != mixture.stdevs.size() ||
        mixture.means.size() != mixture.weights.size())
        throw std::invalid_argument("The means, stdevs and weights differ in size.");
    if (!(initial_price > 0.0))
        throw std::invalid_argument("The initial price must be positive.");
    if (ticks_per_hour <= 0)
        throw std::invalid_argument("The ticks per hour must be positive.");

    if (from_process_type == to_process_type)
        return mixture;

    // Price units are what the arithmetic and Ornstein-Uhlenbeck engines read;
    // the geometric engine's log-returns are that over the price.
    const auto in_price = [&](double value) {
        return from_process_type == geometric ? value * initial_price : value;
    };
    const auto out_of_price = [&](double value) {
        return to_process_type == geometric ? value / initial_price : value;
    };

    fx_mixture out;
    if (to_process_type == ornstein_uhlenbeck || from_process_type == ornstein_uhlenbeck) {
        if (mixture.means.empty())
            return out;
        out.means.push_back(0.0);
        out.stdevs.push_back(out_of_price(in_price(mixture.stdevs.front())));
        out.weights.push_back(to_process_type == ornstein_uhlenbeck
                                  ? kappa_for_half_life(default_half_life_minutes, ticks_per_hour)
                                  : 1.0);
        return out;
    }

    for (std::size_t i = 0; i < mixture.means.size(); ++i) {
        out.means.push_back(out_of_price(in_price(mixture.means[i])));
        out.stdevs.push_back(out_of_price(in_price(mixture.stdevs[i])));
        out.weights.push_back(mixture.weights[i]);
    }
    return out;
}

}
