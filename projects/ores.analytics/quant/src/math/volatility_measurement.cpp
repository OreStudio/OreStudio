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
#include "ores.analytics.quant/math/volatility_measurement.hpp"
#include <algorithm>
#include <cmath>
#include <format>
#include <optional>

namespace ores::analytics::quant::math {

namespace {

/// Whether a level of @p transform's series is an observation.
bool is_observable(double level, volatility_transform transform) {
    if (!std::isfinite(level))
        return false;
    return transform == volatility_transform::difference || level > 0.0;
}

}

dated_returns returns_of(std::span<const dated_level> levels, volatility_transform transform) {
    dated_returns out;
    std::optional<dated_level> previous;
    for (const auto& level : levels) {
        if (!is_observable(level.level, transform))
            continue;
        if (previous) {
            const double value = transform == volatility_transform::log_return ?
                                     std::log(level.level) - std::log(previous->level) :
                                     level.level - previous->level;
            out.returns.push_back(value);
            out.dates.push_back(level.date);
        }
        previous = level;
    }
    return out;
}

int measurement_window(double years, const measurement_settings& settings) {
    // A level is a sample variance, which needs two returns whatever the settings say.
    return std::max({2,
                     settings.minimum_window,
                     static_cast<int>(std::lround(years * settings.periods_per_year))});
}

std::expected<series_measurement, std::string>
measure_series(std::span<const double> returns, const measurement_settings& settings) {
    const auto moments = moments_of(returns);
    if (!moments)
        return std::unexpected(moments.error());
    return series_measurement{.moments = *moments, .shape = shrink(*moments, settings.prior)};
}

std::expected<pillar_measurement, std::string> measure_pillar(
    std::span<const double> returns, double years, const measurement_settings& settings) {
    if (!(settings.periods_per_year > 0.0) || !std::isfinite(settings.periods_per_year))
        return std::unexpected("the periods in a year must be positive and finite");
    if (!(years > 0.0) || !(years <= maximum_pillar_years))
        return std::unexpected(std::format("the pillar must be positive and at most {} years",
                                           static_cast<int>(maximum_pillar_years)));
    const int window = measurement_window(years, settings);
    const auto level = realised_volatility(returns, window, settings.periods_per_year);
    if (!level)
        return std::unexpected(level.error());
    return pillar_measurement{.window = window, .level = *level * settings.level_multiplier};
}

}
