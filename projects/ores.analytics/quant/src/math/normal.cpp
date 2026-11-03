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
#include "ores.analytics.quant/math/normal.hpp"
#include <cmath>
#include <limits>
#include <numbers>

namespace ores::analytics::quant::math {

double normal_pdf(double x) noexcept {
    return std::exp(-0.5 * x * x) * (std::numbers::inv_sqrtpi / std::numbers::sqrt2);
}

double normal_cdf(double x) noexcept {
    return 0.5 * std::erfc(-x / std::numbers::sqrt2);
}

double normal_quantile(double p) noexcept {
    if (!(p >= 0.0 && p <= 1.0))
        return std::numeric_limits<double>::quiet_NaN();
    if (p == 0.0)
        return -std::numeric_limits<double>::infinity();
    if (p == 1.0)
        return std::numeric_limits<double>::infinity();

    const double q = p - 0.5;
    if (std::abs(q) <= 0.425) {
        const double r = 0.180625 - q * q;
        const double num = ((
            ((((((2509.0809287301226727 * r + 33430.575583588128105) * r + 67265.770927008700853) *
                    r +
                45921.953931549871457) *
                   r +
               13731.693765509461125) *
                  r +
              1971.5909503065514427) *
                 r +
             133.14166789178437745) *
                r +
            3.387132872796366608));
        const double den =
            ((((((((5226.495278852545925 * r + 28729.085735721942674) * r + 39307.89580009271061) *
                      r +
                  21213.794301586595867) *
                     r +
                 5394.1960214247511077) *
                    r +
                687.1870074920579083) *
                   r +
               42.313330701600911252) *
                  r +
              1.0));
        return q * num / den;
    }

    double r = std::sqrt(-std::log(q < 0.0 ? p : 1.0 - p));
    double value = 0.0;
    if (r <= 5.0) {
        r -= 1.6;
        const double num = (((((((7.7454501427834140764e-4 * r + 0.0227238449892691845833) * r +
                                 0.24178072517745061177) *
                                    r +
                                1.27045825245236838258) *
                                   r +
                               3.64784832476320460504) *
                                  r +
                              5.7694972214606914055) *
                                 r +
                             4.6303378461565452959) *
                                r +
                            1.42343711074968357734);
        const double den = (((((((1.05075007164441684324e-9 * r + 5.475938084995344946e-4) * r +
                                 0.0151986665636164571966) *
                                    r +
                                0.14810397642748007459) *
                                   r +
                               0.68976733498510000455) *
                                  r +
                              1.6763848301838038494) *
                                 r +
                             2.05319162663775882187) *
                                r +
                            1.0);
        value = num / den;
    } else {
        r -= 5.0;
        const double num = (((((((2.01033439929228813265e-7 * r + 2.71155556874348757815e-5) * r +
                                 0.0012426609473880784386) *
                                    r +
                                0.026532189526576123093) *
                                   r +
                               0.29656057182850489123) *
                                  r +
                              1.7848265399172913358) *
                                 r +
                             5.4637849111641143699) *
                                r +
                            6.6579046435011037772);
        const double den = (((((((2.04426310338993978564e-15 * r + 1.4215117583164458887e-7) * r +
                                 1.8463183175100546818e-5) *
                                    r +
                                7.868691311456132591e-4) *
                                   r +
                               0.0148753612908506148525) *
                                  r +
                              0.13692988092273580531) *
                                 r +
                             0.59983220655588793769) *
                                r +
                            1.0);
        value = num / den;
    }
    return q < 0.0 ? -value : value;
}

}
