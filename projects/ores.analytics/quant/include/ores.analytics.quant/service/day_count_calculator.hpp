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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_DAY_COUNT_CALCULATOR_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_DAY_COUNT_CALCULATOR_HPP

#include "ores.analytics.quant/export.hpp"
#include <chrono>
#include <string>

namespace ores::analytics::quant::service {

/**
 * @brief The day-count conventions this engine implements -- a strongly
 * typed subset of ores.refdata's day_count_fraction_type.code.
 *
 * Only the conventions the bootstrapping engine's own acceptance criteria
 * (single-segment, two-segment/FOMC-style, Projection-off-Funding curves)
 * actually exercise are represented here -- A365F is treated identically
 * to A365 (the fixed/floating-leap-year distinction between them is not
 * modelled), and THIRTY_360 (day_count_fraction_type.code "30/360" --
 * renamed since "/" is not a valid enumerator character) is the bond-basis
 * approximation, not the ISDA/ISMA/AFB Actual/Actual variants or 30E/360.
 * Every other day_count_fraction_type.code has no enumerator, and
 * ores.refdata.core's quant_codes mapping refuses it rather than mapping
 * it to the nearest implemented one.
 */
enum class day_count_convention_code { A360, A365, A365F, THIRTY_360 };

/**
 * @brief Converts a date interval into a year fraction under a
 * day-count convention.
 */
class ORES_ANALYTICS_QUANT_EXPORT day_count_calculator {
public:
    /**
     * @brief Year fraction from @p start to @p end under @p convention.
     *
     * @param start Interval start date; must not be after @p end.
     * @param end Interval end date.
     */
    static double year_fraction(std::chrono::year_month_day start,
                                std::chrono::year_month_day end,
                                day_count_convention_code convention);
};

}

#endif
