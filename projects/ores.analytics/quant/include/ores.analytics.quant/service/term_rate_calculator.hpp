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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_TERM_RATE_CALCULATOR_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_TERM_RATE_CALCULATOR_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/service/day_count_calculator.hpp"
#include <chrono>

namespace ores::analytics::quant::service {

/**
 * @brief How a term index's rate is stated from its currency's overnight curve.
 *
 * Both rules exist because a term index no publisher quotes still needs a rate
 * on the days a trade fixes. Which one a given index takes is data on its curve
 * config, not a property of this calculator.
 */
enum class term_rate_rule {
    /**
     * @brief The overnight rate compounded over the term.
     *
     * A curve's discount factors already carry that compounding: over a term,
     * P(start) / P(end) is the product of one plus each overnight rate times
     * its own day count, so the annualised compounded rate is
     * (P(start) / P(end) - 1) / year_fraction. This is how SOFR 1M and IBR 3M
     * are stated.
     */
    compounded_overnight,
    /**
     * @brief The overnight rate plus a stated spread.
     *
     * A market that quotes no term index still publishes the fixings its
     * participants agree on, as a spread over the overnight average. This is
     * how EYTIBOR 3M, WIBOR 6M and the generic indices are stated.
     */
    overnight_plus_spread
};

/**
 * @brief States a term index's rate from the overnight curve of its currency.
 *
 * Deliberately data-oriented and free of any curve object: the caller reads the
 * two discount factors and the overnight rate from wherever it holds them, so
 * this is the same arithmetic for a run's board, a published curve read back
 * from the store, or a test.
 */
class ORES_ANALYTICS_QUANT_EXPORT term_rate_calculator {
public:
    /**
     * @brief The term index's rate over [start, end].
     *
     * @param overnight_rate The overnight index's rate at @p start. Read only
     *        by the overnight-plus-spread rule; the compounded rule takes its
     *        level from the discount factors alone.
     * @param discount_factor_start P(start); must be > 0.
     * @param discount_factor_end P(end); must be > 0.
     * @param convention The day-count convention of the overnight index, whose
     *        year fraction annualises both rules.
     * @param rule Which rule states the rate.
     * @param spread_basis_points The spread the overnight-plus-spread rule
     *        adds, in basis points. Ignored by the compounded rule.
     * @throws std::invalid_argument if a discount factor is not positive, if
     *         @p end is not after @p start, or if the term's year fraction is
     *         not positive.
     */
    static double rate(double overnight_rate,
                       double discount_factor_start,
                       double discount_factor_end,
                       std::chrono::year_month_day start,
                       std::chrono::year_month_day end,
                       day_count_convention_code convention,
                       term_rate_rule rule,
                       double spread_basis_points);
};

}

#endif
