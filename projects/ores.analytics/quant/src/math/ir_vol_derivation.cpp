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
#include "ores.analytics.quant/math/ir_vol_derivation.hpp"
#include <algorithm>
#include <cmath>
#include <format>

namespace ores::analytics::quant::math {

namespace {

/**
 * The mean reversion below which the driftless limit is read instead of the model's own expression,
 * where the two divisions would lose their precision or divide by zero.
 */
constexpr double minimum_mean_reversion = 1e-8;

/// The skewness the smile's slope carries, over six, as the Gram-Charlier correction states it.
constexpr double skew_over_six = 6.0;

}

std::string_view name_of(short_rate_model model) {
    switch (model) {
        case short_rate_model::vasicek:
            return "vasicek";
        case short_rate_model::hull_white:
            return "hull_white";
    }
    return {};
}

std::optional<short_rate_model> short_rate_model_named(std::string_view name) {
    if (name == "vasicek")
        return short_rate_model::vasicek;
    if (name == "hull_white")
        return short_rate_model::hull_white;
    return std::nullopt;
}

std::expected<double, std::string> projection_factor(swaption_pillar pillar,
                                                     short_rate_projection model) {
    if (!(pillar.expiry_years > 0.0) || !std::isfinite(pillar.expiry_years))
        return std::unexpected("the expiry must be positive and finite");
    if (!(pillar.term_years > 0.0) || !std::isfinite(pillar.term_years))
        return std::unexpected("the term must be positive and finite");
    if (!std::isfinite(model.mean_reversion) || model.mean_reversion < 0.0)
        return std::unexpected("the mean reversion must be finite and not negative");

    const double a = model.mean_reversion;
    const bool has_reversion = a > minimum_mean_reversion;
    const double variance = has_reversion ?
                                (1.0 - std::exp(-2.0 * a * pillar.expiry_years)) / (2.0 * a) :
                                pillar.expiry_years;
    const double average =
        has_reversion ? (1.0 - std::exp(-a * pillar.term_years)) / (a * pillar.term_years) : 1.0;
    return std::sqrt(variance) * average;
}

std::expected<ir_pillar_vol, std::string>
atm_vol_of(double level, int window, swaption_pillar pillar, short_rate_projection model) {
    if (!std::isfinite(level) || level < 0.0)
        return std::unexpected("the level must be finite and not negative");
    const auto factor = projection_factor(pillar, model);
    if (!factor)
        return std::unexpected(factor.error());
    return ir_pillar_vol{
        .window = window, .level = level, .projection = *factor, .atm = level * *factor};
}

normal_skew_smile smile_of(double atm_volatility, return_shape shape) noexcept {
    return {.alpha = atm_volatility, .skewness = shape.skewness};
}

double smile_at(const normal_skew_smile& smile,
                double forward,
                double strike,
                double expiry_years) noexcept {
    if (!(smile.alpha > 0.0) || !(expiry_years > 0.0) || !std::isfinite(forward) ||
        !std::isfinite(strike))
        return smile.alpha;
    const double standardised = (strike - forward) / (smile.alpha * std::sqrt(expiry_years));
    return std::max(smile.alpha * (1.0 + smile.skewness * standardised / skew_over_six), 0.0);
}

std::expected<ir_term_matrix, std::string> derive_term_matrix(std::span<const double> returns,
                                                              double term_years,
                                                              std::span<const double> expiry_years,
                                                              const measurement_settings& settings,
                                                              short_rate_projection model) {
    if (!(settings.periods_per_year > 0.0) || !std::isfinite(settings.periods_per_year))
        return std::unexpected("the periods in a year must be positive and finite");
    const auto series = measure_series(returns, settings);
    if (!series)
        return std::unexpected(series.error());

    ir_term_matrix out{.series = *series, .cells = {}};
    out.cells.reserve(expiry_years.size());
    for (const double years : expiry_years) {
        const auto measured = measure_pillar(returns, years, settings);
        if (!measured) {
            out.cells.emplace_back(std::unexpect, measured.error());
            continue;
        }
        out.cells.emplace_back(atm_vol_of(measured->level,
                                          measured->window,
                                          {.expiry_years = years, .term_years = term_years},
                                          model));
    }
    return out;
}

}
