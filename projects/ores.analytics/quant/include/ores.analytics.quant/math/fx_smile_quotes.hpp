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
#ifndef ORES_ANALYTICS_QUANT_MATH_FX_SMILE_QUOTES_HPP
#define ORES_ANALYTICS_QUANT_MATH_FX_SMILE_QUOTES_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/fx_delta.hpp"
#include <boost/compat/function_ref.hpp>
#include <expected>
#include <span>
#include <string>

namespace ores::analytics::quant::math {

/**
 * @brief The implied volatility smile of a return with a given volatility,
 * skewness and excess kurtosis over an expiry.
 *
 * Backus, Foresi, Li and Wu (1997), equation 16: a Gram-Charlier expansion of the
 * return's density gives v(d) = sigma (1 - skewness d / 6 - excess_kurtosis
 * (1 - d^2) / 24), with d = (log(F / K) + sigma^2 t / 2) / (sigma sqrt(t)) taken
 * at sigma, so the smile is explicit in the strike. The moments are those of the
 * return over the expiry, not of a day.
 */
struct ORES_ANALYTICS_QUANT_EXPORT gram_charlier_smile final {
    /// The annualised volatility of the return over the expiry.
    double sigma;
    double skewness;
    double excess_kurtosis;
    /// The expiry, in years.
    double years;

    [[nodiscard]] double volatility_at_d(double d) const noexcept;
    [[nodiscard]] double volatility_at_strike(double forward, double strike) const noexcept;
};

/// Which side a risk reversal is quoted in favour of: call minus put, or put minus call.
enum class fx_risk_reversal_favour { call, put };

/**
 * @brief How a butterfly is quoted.
 *
 * A smile butterfly is the mean of the two wing volatilities less the ATM. A broker
 * butterfly is the single volatility, over the ATM, at which a strangle struck at the
 * pillar delta prices as it does on the smile (Clark, 2011, section 3.7).
 */
enum class fx_butterfly_style { smile, broker };

/// The quoting convention of one expiry of a pair.
struct fx_quote_convention final {
    fx_atm_type atm;
    fx_delta_type delta;
    fx_risk_reversal_favour favour;
    fx_butterfly_style butterfly;
};

/// The ATM volatility and the risk reversals and butterflies at the 25 and 10 delta pillars.
struct fx_smile_quotes final {
    double atm;
    double rr25;
    double bf25;
    double rr10;
    double bf10;
};

/// A smile: the volatility at log(K / F).
using fx_smile_function = boost::compat::function_ref<double(double)>;

/// What a smile quotes at one delta, and the strikes that delta resolves to.
struct fx_wing_reading final {
    double put_strike;
    double call_strike;
    double risk_reversal;
    double butterfly;
};

/// Where a smile puts its at-the-money, and what it quotes there.
struct fx_atm_reading final {
    double strike;
    double volatility;
};

/**
 * @brief The quotes of any smile in a convention, at the given deltas.
 *
 * A fit calls this many times, so the caller owns the wings and a successful read allocates
 * nothing. Refuses a smile that is not positive where it is read, and a pillar or butterfly
 * with no solution.
 *
 * @param level A volatility near the smile's at-the-money, where the searches start.
 * @param years The expiry, in years.
 * @param wings One entry for each delta, written in the order of the deltas.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<fx_atm_reading, std::string>
read_smile(fx_smile_function smile,
           double level,
           double years,
           const fx_market& market,
           const fx_quote_convention& convention,
           std::span<const double> deltas,
           std::span<fx_wing_reading> wings);

/**
 * @brief The quotes of a smile in a convention.
 *
 * The ATM strike is solved on the smile, so a delta-neutral ATM is neutral at the
 * smile's own volatility there. Each pillar strike is solved so that its delta, at
 * the smile's volatility at that strike, is the pillar's. Refuses a smile that is
 * not positive where it is read, and a pillar or butterfly with no solution.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<fx_smile_quotes, std::string>
quote_smile(const gram_charlier_smile& smile,
            const fx_market& market,
            const fx_quote_convention& convention);

}

#endif
