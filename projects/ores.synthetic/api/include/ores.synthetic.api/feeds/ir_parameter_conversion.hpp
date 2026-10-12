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
#ifndef ORES_SYNTHETIC_API_FEEDS_IR_PARAMETER_CONVERSION_HPP
#define ORES_SYNTHETIC_API_FEEDS_IR_PARAMETER_CONVERSION_HPP

#include "ores.synthetic.api/export.hpp"
#include <map>
#include <optional>
#include <string>

namespace ores::synthetic::feed {

/**
 * @brief The four questions the simple view of an IR feed asks.
 *
 * Rates are decimals: 0.03 is 3%. The half-life is in years. The yearly move
 * is the size, in basis points, of a typical one-year change in the short
 * rate.
 */
struct ir_simple_view {
    double start_rate = 0.0;
    double long_run_rate = 0.0;
    double half_life_years = 0.0;
    double yearly_move_bp = 0.0;
};

/**
 * @brief What the simple view makes of an engine's parameters.
 *
 * @p view is empty when the parameters have no simple form: a reversion speed
 * of zero never halves the distance to the long-run level, and a
 * Two-Factor Gaussian feed whose second factor or correlation differs from
 * the simple view's choices has more than the four questions can say.
 * @p reason then says why. @p caution is non-empty when the parameters have a
 * view but a risk worth saying: "touches_zero" for a Cox-Ingersoll-Ross
 * volatility so high for its reversion speed (σ² above 2κθ) that the rate can
 * reach zero.
 */
struct ir_simple_reading {
    std::optional<ir_simple_view> view;
    std::string reason;
    std::string caution;
};

/**
 * @brief The engine parameters a simple view stands for, by parameter name.
 *
 * The reversion speed κ is ln 2 over the half-life. The yearly move becomes
 * σ in the unit the engine reads: a rate move for Vasicek and Hull-White; a
 * rate move divided by the square root of the long-run level for
 * Cox-Ingersoll-Ross, whose σ scales with √r; a relative move for
 * Black-Karasinski, whose long-run level is also written as a logarithm. The
 * Two-Factor Gaussian engine gets a second factor that reverts at 60% of the
 * first speed with half its volatility, uncorrelated, split so the total move
 * is the yearly move.
 *
 * Throws std::invalid_argument for an unknown engine, a half-life that is not
 * positive, a negative rate or move, or a long-run level that is not positive
 * for an engine that divides by it.
 */
ORES_SYNTHETIC_API_EXPORT std::map<std::string, double>
ir_parameters_from_simple(const std::string& process_type, const ir_simple_view& view);

/**
 * @brief The simple view of an engine's parameters, or why there is none.
 *
 * The inverse of ir_parameters_from_simple. Throws std::invalid_argument for
 * an unknown engine or a missing parameter.
 */
ORES_SYNTHETIC_API_EXPORT ir_simple_reading
ir_simple_from_parameters(const std::string& process_type,
                          const std::map<std::string, double>& parameters);

}

#endif
