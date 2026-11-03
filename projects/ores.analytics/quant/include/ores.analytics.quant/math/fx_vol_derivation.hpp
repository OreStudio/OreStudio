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
#ifndef ORES_ANALYTICS_QUANT_MATH_FX_VOL_DERIVATION_HPP
#define ORES_ANALYTICS_QUANT_MATH_FX_VOL_DERIVATION_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/fx_smile_quotes.hpp"
#include "ores.analytics.quant/math/return_moments.hpp"
#include <expected>
#include <span>
#include <string>
#include <vector>

namespace ores::analytics::quant::math {

/// One expiry to quote: its label, its length and its convention.
struct fx_expiry final {
    std::string label;
    double years;
    fx_quote_convention convention;
    /// The forward the quotes are read against; with no rates, the spot itself.
    fx_market market{.spot = 1.0, .domestic_discount = 1.0, .foreign_discount = 1.0};
};

/// The authored choices of a derivation.
struct fx_derivation_settings final {
    shape_prior prior{.shape = {.skewness = 0.0, .excess_kurtosis = 1.0}, .weight = 500.0};
    /// Scales the realised level, for the volatility risk premium no free source publishes.
    double level_multiplier = 1.0;
    double trading_days_per_year = 252.0;
    /// The shortest window the level is read over, in returns.
    int minimum_window = 21;
};

/// What an expiry's quotes were derived from, beside the quotes.
struct derived_fx_expiry final {
    std::string label;
    fx_smile_quotes quotes;
    gram_charlier_smile smile;
    /// The returns the level was read over.
    int window;
};

/// The quotes of every expiry, each one or why it could not be derived.
struct fx_derivation final {
    return_moments moments;
    return_shape shape;
    std::vector<std::expected<derived_fx_expiry, std::string>> expiries;
};

/**
 * @brief The quotes of a pair inferred from its daily log returns.
 *
 * The level of an expiry is the realised volatility over the last window of the
 * expiry's length, never shorter than the minimum, times the multiplier. The shape
 * is read over every return held and shrunk toward the prior, then scaled to the
 * expiry's length in trading days and held inside the Gram-Charlier region. The
 * quotes are the Backus, Foresi, Li and Wu smile read in the expiry's convention.
 * Refuses a sample with no moments; an expiry whose window the sample does not
 * cover is refused alone.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<fx_derivation, std::string>
derive_fx_quotes(std::span<const double> daily_log_returns,
                 std::span<const fx_expiry> expiries,
                 const fx_derivation_settings& settings);

}

#endif
