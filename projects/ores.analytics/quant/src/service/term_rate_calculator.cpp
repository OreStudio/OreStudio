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
#include "ores.analytics.quant/service/term_rate_calculator.hpp"
#include "ores.analytics.quant/service/curve_instrument_pricer.hpp"
#include <stdexcept>

namespace ores::analytics::quant::service {

double term_rate_calculator::rate(double overnight_rate,
                                  double discount_factor_start,
                                  double discount_factor_end,
                                  std::chrono::year_month_day start,
                                  std::chrono::year_month_day end,
                                  day_count_convention_code convention,
                                  term_rate_rule rule,
                                  double spread_basis_points) {
    if (discount_factor_start <= 0.0 || discount_factor_end <= 0.0)
        throw std::invalid_argument("term_rate_calculator: discount factors must be positive");

    const double year_fraction = day_count_calculator::year_fraction(start, end, convention);
    if (year_fraction <= 0.0)
        throw std::invalid_argument(
            "term_rate_calculator: the term must span a positive year fraction");

    switch (rule) {
        case term_rate_rule::compounded_overnight:
            return curve_instrument_pricer::fra_rate(
                discount_factor_start, discount_factor_end, year_fraction);
        case term_rate_rule::overnight_plus_spread:
            return overnight_rate + spread_basis_points / 10000.0;
    }
    throw std::invalid_argument("term_rate_calculator: unknown term rate rule");
}

}
