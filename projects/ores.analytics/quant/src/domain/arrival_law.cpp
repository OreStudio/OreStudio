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
#include "ores.analytics.quant/domain/arrival_law.hpp"
#include "ores.analytics.quant/domain/process_parameter_validation.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>

namespace ores::analytics::quant::domain {

namespace {

template <class... Ts>
struct overloaded : Ts... {
    using Ts::operator()...;
};

}

arrival_law make_arrival_law(const arrival_spec& spec, math::rng_stream& stream) {
    const auto validation = validate_arrival_process_parameters(
        spec.mode, spec.ticks_per_hour, spec.branching_ratio, spec.decay);
    if (!validation.valid)
        throw std::invalid_argument("make_arrival_law: " + validation.message);

    constexpr double seconds_per_hour = 3600.0;
    const double rate = spec.ticks_per_hour / seconds_per_hour;

    if (spec.mode == "periodic")
        return periodic_law{.rate = rate, .offset = math::open_uniform(stream)};
    if (spec.mode == "poisson")
        return poisson_law{.rate = rate};
    if (spec.mode != "hawkes")
        throw std::invalid_argument("make_arrival_law: unrecognised mode '" + spec.mode + "'");

    // The stationary intensity is the mean rate, so the stationary excess is the
    // part of it the baseline does not supply.
    const double n = spec.branching_ratio;
    return hawkes_law{.baseline = rate * (1.0 - n),
                      .jump = n * spec.decay,
                      .decay = spec.decay,
                      .excess = rate * n};
}

double draw_gap(const arrival_law& law, math::rng_stream& stream) {
    return std::visit(
        overloaded{
            [](const periodic_law& p) { return p.offset / p.rate; },
            [&](const poisson_law& p) { return -std::log(math::open_uniform(stream)) / p.rate; },
            [&](const hawkes_law& h) {
                // The earlier of a baseline arrival and one from the fading excess
                // (Dassios and Zhao, 2013). Both uniforms are drawn every time, so
                // a stream's position does not depend on the state.
                const double baseline_gap = -std::log(math::open_uniform(stream)) / h.baseline;
                const double d = 1.0 + h.decay * std::log(math::open_uniform(stream)) / h.excess;
                const double excess_gap = h.excess > 0.0 && d > 0.0 ?
                                              -std::log(d) / h.decay :
                                              std::numeric_limits<double>::infinity();
                return std::min(baseline_gap, excess_gap);
            },
        },
        law);
}

void record_event(arrival_law& law, double gap) {
    std::visit(overloaded{
                   [](periodic_law& p) { p.offset = 1.0; },
                   [](poisson_law&) {},
                   [&](hawkes_law& h) { h.excess = h.excess * std::exp(-h.decay * gap) + h.jump; },
               },
               law);
}

}
