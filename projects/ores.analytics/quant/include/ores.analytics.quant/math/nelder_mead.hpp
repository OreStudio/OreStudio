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
#ifndef ORES_ANALYTICS_QUANT_MATH_NELDER_MEAD_HPP
#define ORES_ANALYTICS_QUANT_MATH_NELDER_MEAD_HPP

#include <algorithm>
#include <array>
#include <cmath>
#include <cstddef>
#include <utility>

namespace ores::analytics::quant::math {

/// Where a simplex search stopped, what it found there, and why.
template <std::size_t N>
struct nelder_mead_result final {
    std::array<double, N> x;
    double value;
    int iterations;
    /// Whether the simplex shrank within the tolerance, rather than running out of iterations.
    bool converged;
};

/// The stopping rule of a simplex search.
struct nelder_mead_options final {
    /// The simplex stops when its values differ by less than this, relative to the best.
    double tolerance = 1e-12;
    int max_iterations = 5000;
};

/**
 * @brief Minimise a function of N variables by the Nelder-Mead simplex method.
 *
 * The method needs no derivative, which suits a likelihood with a boundary: a point
 * outside the admissible region is given a large value, and the simplex moves away
 * from it. The standard coefficients are used: reflection 1, expansion 2, contraction
 * and shrink one half (Nelder and Mead, 1965, The Computer Journal 7(4)). The simplex
 * is N + 1 arrays held on the stack, so an iteration allocates nothing, and a run is
 * a pure function of the function, the start and the step.
 *
 * @param f A callable taking a const std::array<double, N>& and returning a double.
 * @param start The first vertex.
 * @param step The offset of each further vertex along its own axis.
 */
template <std::size_t N, class F>
nelder_mead_result<N> nelder_mead(F&& f,
                                  const std::array<double, N>& start,
                                  const std::array<double, N>& step,
                                  const nelder_mead_options& options = {}) {
    using point = std::array<double, N>;
    std::array<point, N + 1> vertex{};
    std::array<double, N + 1> value{};
    vertex[0] = start;
    for (std::size_t i = 0; i < N; ++i) {
        vertex[i + 1] = start;
        vertex[i + 1][i] += step[i];
    }
    for (std::size_t i = 0; i <= N; ++i)
        value[i] = f(vertex[i]);

    const auto along = [](const point& from, const point& through, double t) {
        point out{};
        for (std::size_t i = 0; i < N; ++i)
            out[i] = from[i] + t * (through[i] - from[i]);
        return out;
    };

    std::array<std::size_t, N + 1> order{};
    int iteration = 0;
    for (; iteration < options.max_iterations; ++iteration) {
        for (std::size_t i = 0; i <= N; ++i)
            order[i] = i;
        std::ranges::sort(order, [&](std::size_t a, std::size_t b) { return value[a] < value[b]; });
        const std::size_t best = order.front();
        const std::size_t worst = order.back();
        const std::size_t second = order[N - 1];

        const double spread = value[worst] - value[best];
        if (spread <= options.tolerance * (std::abs(value[best]) + options.tolerance))
            return {vertex[best], value[best], iteration, true};

        point centroid{};
        for (std::size_t i = 0; i <= N; ++i) {
            if (i == worst)
                continue;
            for (std::size_t d = 0; d < N; ++d)
                centroid[d] += vertex[i][d] / static_cast<double>(N);
        }

        const point reflected = along(centroid, vertex[worst], -1.0);
        const double reflected_value = f(reflected);
        if (reflected_value < value[best]) {
            const point expanded = along(centroid, vertex[worst], -2.0);
            const double expanded_value = f(expanded);
            if (expanded_value < reflected_value) {
                vertex[worst] = expanded;
                value[worst] = expanded_value;
            } else {
                vertex[worst] = reflected;
                value[worst] = reflected_value;
            }
            continue;
        }
        if (reflected_value < value[second]) {
            vertex[worst] = reflected;
            value[worst] = reflected_value;
            continue;
        }
        const bool outside = reflected_value < value[worst];
        const point contracted =
            outside ? along(centroid, reflected, 0.5) : along(centroid, vertex[worst], 0.5);
        const double contracted_value = f(contracted);
        if (contracted_value < std::min(reflected_value, value[worst])) {
            vertex[worst] = contracted;
            value[worst] = contracted_value;
            continue;
        }
        for (std::size_t i = 0; i <= N; ++i) {
            if (i == best)
                continue;
            vertex[i] = along(vertex[best], vertex[i], 0.5);
            value[i] = f(vertex[i]);
        }
    }
    const auto best = static_cast<std::size_t>(std::ranges::min_element(value) - value.begin());
    return {vertex[best], value[best], iteration, false};
}

}

#endif
