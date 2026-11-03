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
#ifndef ORES_ANALYTICS_QUANT_MATH_BRENT_HPP
#define ORES_ANALYTICS_QUANT_MATH_BRENT_HPP

#include <algorithm>
#include <cmath>
#include <expected>
#include <limits>
#include <string>

namespace ores::analytics::quant::math {

/// The stopping rule of a root search.
struct brent_options final {
    /// The search stops when the bracket is narrower than this, in the variable.
    double tolerance = 1e-12;
    int max_iterations = 200;
};

/**
 * @brief A root of f in [lo, hi], where f(lo) and f(hi) differ in sign.
 *
 * Brent's method (Algorithms for Minimization without Derivatives, 1973,
 * chapter 4): inverse quadratic interpolation where it makes progress, bisection
 * where it does not, so it never does worse than bisection. Refuses a bracket
 * whose ends have the same sign, and a search that runs out of iterations.
 */
template <class F>
std::expected<double, std::string>
brent_root(F&& f, double lo, double hi, const brent_options& options = {}) {
    double a = lo;
    double b = hi;
    double fa = f(a);
    double fb = f(b);
    if (fa == 0.0)
        return a;
    if (fb == 0.0)
        return b;
    if (!(std::isfinite(fa) && std::isfinite(fb)) || (fa > 0.0) == (fb > 0.0))
        return std::unexpected("the bracket does not hold a sign change");

    double c = a;
    double fc = fa;
    double d = b - a;
    double e = d;
    for (int i = 0; i < options.max_iterations; ++i) {
        if ((fb > 0.0) == (fc > 0.0)) {
            c = a;
            fc = fa;
            d = b - a;
            e = d;
        }
        if (std::abs(fc) < std::abs(fb)) {
            a = b;
            b = c;
            c = a;
            fa = fb;
            fb = fc;
            fc = fa;
        }
        const double tol =
            2.0 * std::numeric_limits<double>::epsilon() * std::abs(b) + 0.5 * options.tolerance;
        const double m = 0.5 * (c - b);
        if (std::abs(m) <= tol || fb == 0.0)
            return b;
        if (std::abs(e) >= tol && std::abs(fa) > std::abs(fb)) {
            double p = 0.0;
            double q = 0.0;
            const double s = fb / fa;
            if (a == c) {
                p = 2.0 * m * s;
                q = 1.0 - s;
            } else {
                const double qa = fa / fc;
                const double r = fb / fc;
                p = s * (2.0 * m * qa * (qa - r) - (b - a) * (r - 1.0));
                q = (qa - 1.0) * (r - 1.0) * (s - 1.0);
            }
            if (p > 0.0)
                q = -q;
            else
                p = -p;
            if (2.0 * p < std::min(3.0 * m * q - std::abs(tol * q), std::abs(e * q))) {
                e = d;
                d = p / q;
            } else {
                d = m;
                e = m;
            }
        } else {
            d = m;
            e = m;
        }
        a = b;
        fa = fb;
        b += std::abs(d) > tol ? d : (m > 0.0 ? tol : -tol);
        fb = f(b);
        if (!std::isfinite(fb))
            return std::unexpected("the function is not finite inside the bracket");
    }
    return std::unexpected("the root search ran out of iterations");
}

}

#endif
