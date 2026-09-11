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
#include "ores.analytics.quant/service/curve_instrument_pricer.hpp"
#include <stdexcept>

namespace ores::analytics::quant::service {

double curve_instrument_pricer::deposit_rate(double discount_factor, double year_fraction) {
    if (discount_factor <= 0.0)
        throw std::invalid_argument("curve_instrument_pricer: discount_factor must be positive");
    if (year_fraction <= 0.0)
        throw std::invalid_argument("curve_instrument_pricer: year_fraction must be positive");

    return (1.0 / discount_factor - 1.0) / year_fraction;
}

double curve_instrument_pricer::fra_rate(double discount_factor_start,
                                         double discount_factor_end,
                                         double accrual_fraction) {
    if (discount_factor_start <= 0.0 || discount_factor_end <= 0.0)
        throw std::invalid_argument("curve_instrument_pricer: discount factors must be positive");
    if (accrual_fraction <= 0.0)
        throw std::invalid_argument("curve_instrument_pricer: accrual_fraction must be positive");

    return (discount_factor_start / discount_factor_end - 1.0) / accrual_fraction;
}

double curve_instrument_pricer::swap_par_rate(double discount_factor_start,
                                              double discount_factor_end,
                                              const std::vector<double>& fixed_leg_discount_factors,
                                              const std::vector<double>& accrual_fractions) {
    if (discount_factor_start <= 0.0 || discount_factor_end <= 0.0)
        throw std::invalid_argument("curve_instrument_pricer: discount factors must be positive");
    if (fixed_leg_discount_factors.empty())
        throw std::invalid_argument(
            "curve_instrument_pricer: fixed_leg_discount_factors must not be empty");
    if (fixed_leg_discount_factors.size() != accrual_fractions.size())
        throw std::invalid_argument(
            "curve_instrument_pricer: fixed_leg_discount_factors and accrual_fractions must "
            "have the same size");

    double annuity = 0.0;
    for (std::size_t i = 0; i < fixed_leg_discount_factors.size(); ++i) {
        if (fixed_leg_discount_factors[i] <= 0.0)
            throw std::invalid_argument(
                "curve_instrument_pricer: fixed_leg_discount_factors entries must be positive");
        if (accrual_fractions[i] <= 0.0)
            throw std::invalid_argument(
                "curve_instrument_pricer: accrual_fractions entries must be positive");
        annuity += accrual_fractions[i] * fixed_leg_discount_factors[i];
    }

    return (discount_factor_start - discount_factor_end) / annuity;
}

}
