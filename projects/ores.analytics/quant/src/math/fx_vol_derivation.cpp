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
#include "ores.analytics.quant/math/fx_vol_derivation.hpp"
#include <algorithm>
#include <cmath>
#include <string>

namespace ores::analytics::quant::math {

namespace {

/// The longest expiry quoted: past it no history held covers the window.
constexpr double maximum_years = 50.0;

}

std::expected<fx_derivation, std::string>
derive_fx_quotes(std::span<const double> daily_log_returns,
                 std::span<const fx_expiry> expiries,
                 const fx_derivation_settings& settings) {
    if (!(settings.trading_days_per_year > 0.0) || !std::isfinite(settings.trading_days_per_year))
        return std::unexpected("the trading days in a year must be positive and finite");
    const auto moments = moments_of(daily_log_returns);
    if (!moments)
        return std::unexpected(moments.error());
    fx_derivation out{
        .moments = *moments, .shape = shrink(*moments, settings.prior), .expiries = {}};
    out.expiries.reserve(expiries.size());
    for (const auto& expiry : expiries) {
        if (!(expiry.years > 0.0) || !(expiry.years <= maximum_years)) {
            out.expiries.emplace_back(std::unexpect,
                                      expiry.label + ": the expiry must be positive and at most " +
                                          std::to_string(static_cast<int>(maximum_years)) +
                                          " years");
            continue;
        }
        const double days = expiry.years * settings.trading_days_per_year;
        const int window = std::max(settings.minimum_window, static_cast<int>(std::lround(days)));
        const auto level =
            realised_volatility(daily_log_returns, window, settings.trading_days_per_year);
        if (!level) {
            out.expiries.emplace_back(std::unexpect, expiry.label + ": " + level.error());
            continue;
        }
        const auto shape = hold_in_gram_charlier_region(over_days(out.shape, days));
        const gram_charlier_smile smile{.sigma = *level * settings.level_multiplier,
                                        .skewness = shape.skewness,
                                        .excess_kurtosis = shape.excess_kurtosis,
                                        .years = expiry.years};
        const auto quotes = quote_smile(smile, expiry.market, expiry.convention);
        if (!quotes) {
            out.expiries.emplace_back(std::unexpect, expiry.label + ": " + quotes.error());
            continue;
        }
        out.expiries.emplace_back(derived_fx_expiry{
            .label = expiry.label, .quotes = *quotes, .smile = smile, .window = window});
    }
    return out;
}

}
