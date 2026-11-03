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
#ifndef ORES_ANALYTICS_QUANT_MATH_ROOT_HPP
#define ORES_ANALYTICS_QUANT_MATH_ROOT_HPP

#include <boost/math/tools/toms748_solve.hpp>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <exception>
#include <expected>
#include <limits>
#include <string>

namespace ores::analytics::quant::math {

/// The stopping rule of a root search.
struct root_options final {
    /// The bracket is narrow enough within this, absolutely, or a few ulps relatively.
    double tolerance = 1e-14;
    int max_iterations = 200;
};

/**
 * @brief A root of f in [lo, hi], where f(lo) and f(hi) differ in sign.
 *
 * Boost.Math's TOMS 748 (Alefeld, Potra and Shi, 1995), which converges at least as
 * fast as Brent's method. The tolerance is absolute as well as relative, so a root at
 * zero ends. Boost's errors, a bracket without a sign change among them, and a search
 * that runs out of iterations become the reason the root was not found.
 */
template <class F>
std::expected<double, std::string>
find_root(F&& f, double lo, double hi, const root_options& options = {}) {
    const auto close_enough = [&](double a, double b) {
        return std::abs(a - b) <= options.tolerance + 4.0 * std::numeric_limits<double>::epsilon() *
                                                          std::min(std::abs(a), std::abs(b));
    };
    try {
        auto iterations = static_cast<std::uintmax_t>(options.max_iterations);
        const auto [a, b] = boost::math::tools::toms748_solve(f, lo, hi, close_enough, iterations);
        if (iterations >= static_cast<std::uintmax_t>(options.max_iterations))
            return std::unexpected("the root search ran out of iterations");
        return 0.5 * (a + b);
    } catch (const std::exception& e) {
        return std::unexpected(std::string("the root search failed: ") + e.what());
    }
}

}

#endif
