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
#ifndef ORES_ANALYTICS_QUANT_MATH_VOLATILITY_MEASUREMENT_HPP
#define ORES_ANALYTICS_QUANT_MATH_VOLATILITY_MEASUREMENT_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/return_moments.hpp"
#include <chrono>
#include <cstdint>
#include <expected>
#include <span>
#include <string>
#include <utility>
#include <vector>

namespace ores::analytics::quant::math {

/**
 * @file volatility_measurement.hpp
 * @brief What a volatility is measured from, and how much history a pillar needs.
 *
 * A volatility quote has no free source, so it is measured from a history of the thing it
 * describes: a window as long as the pillar, the moments of the returns over that window, a level
 * scale for the risk premium no source publishes, and the shape of the returns for the smile. None
 * of that names an asset class. What does name one is three things, and they are this file's
 * parameters or its caller's:
 *
 * - the transform that turns a level series into returns, which is here;
 * - the coordinates a quote is named by, which belong to the instrument type;
 * - the smile model that turns a level and a shape into quotes, which belongs to the class.
 */

/**
 * @brief How a series of levels becomes the returns a volatility is measured on.
 *
 * A level that keeps a sign has a meaningful ratio; a level that may cross zero has only its
 * change. The choice is the asset class's, and it is the whole of what makes measuring a price
 * different from measuring a rate.
 */
enum class volatility_transform : std::uint8_t {
    /// The change of the logarithm. A level that is not positive is no observation.
    log_return,
    /// The change of the level itself, in whatever unit the series carries.
    difference
};

/// One observation of a series: the day it is dated, and the level.
struct dated_level final {
    std::chrono::year_month_day date;
    double level;
};

/// The returns of a series, oldest first, each with the day it ends on.
struct dated_returns final {
    std::vector<double> returns;
    std::vector<std::chrono::year_month_day> dates;
};

/// The longest pillar a measurement serves: past it no history held covers the window.
inline constexpr double maximum_pillar_years = 50.0;

/**
 * @brief The returns of @p levels by @p transform, one fewer than the observations.
 *
 * A level that is not finite is no observation and is left out, so a return spans the gap rather
 * than reading a substituted value. A log return leaves out a level that is not positive for the
 * same reason: it has no logarithm, and a return invented for it would read as a move.
 */
ORES_ANALYTICS_QUANT_EXPORT dated_returns returns_of(std::span<const dated_level> levels,
                                                     volatility_transform transform);

/**
 * @brief Whether a level of @p transform's series is an observation.
 *
 * A level that is not finite is no observation, and neither is a level that is not positive when
 * the transform takes a logarithm. The one rule returns_of applies, exposed so a caller that
 * streams a series -- a batch that walks it once from the newest level -- reads the same series.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT bool
reads_as_observation(double level, volatility_transform transform) noexcept;

/// The return of @p value from @p previous, by @p transform. Both must be observations.
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT double
return_of(double value, double previous, volatility_transform transform) noexcept;

/// The authored choices of a measurement.
///
/// The transform is not one of them: it is the argument of returns_of, because a setting a caller
/// states and nothing reads is a setting that lies about what it looked at.
struct measurement_settings final {
    shape_prior prior{.shape = {.skewness = 0.0, .excess_kurtosis = 1.0}, .weight = 500.0};
    /// Scales the measured level, for the volatility risk premium no free source publishes.
    double level_multiplier = 1.0;
    /// The returns a year holds, for the annualisation and for a pillar's length.
    double periods_per_year = 252.0;
    /// The shortest window a level is read over, in returns.
    int minimum_window = 21;
};

/**
 * @brief The returns a pillar's level is read over: its length in periods, and never fewer than
 * the settings' minimum window, or than the two a sample variance needs.
 *
 * The one rule for how much history a pillar needs, so the reach a source reports and the window
 * a level is read over cannot disagree.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT int
measurement_window(double years, const measurement_settings& settings);

/// Refused for settings no measurement can read: a year with no periods, or one that is not finite.
ORES_ANALYTICS_QUANT_EXPORT std::expected<void, std::string>
validate_settings(const measurement_settings& settings);

/**
 * @brief The level a sample variance states: its root over the periods in a year, times the
 * settings' multiplier.
 *
 * The one rule that turns a variance into a level, so the window a batch reads from the sums of a
 * series and the window the engine reads from the sample itself cannot disagree.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT double
level_of_variance(double variance, const measurement_settings& settings) noexcept;

/// What a series says about its own shape, once, however many pillars are measured from it.
struct series_measurement final {
    return_moments moments;
    /// The whole sample's moments, shrunk toward the settings' prior.
    return_shape shape;
};

/**
 * @brief The shape of @p returns: their moments, shrunk toward the prior.
 *
 * The shape is read over every return the caller holds and not over a window, because the
 * standard error of a skewness hardly falls with a longer window, while the level's does. Refused
 * for a sample of fewer than four returns or of no spread.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<series_measurement, std::string>
measure_series(std::span<const double> returns, const measurement_settings& settings);

/// What one pillar's level read.
struct pillar_measurement final {
    /// The returns the level was read over.
    int window;
    /// The annualised level over the window, times the settings' multiplier. In a log-return
    /// series it is a volatility, and in a difference series it is the annualised change in the
    /// series' own unit.
    double level;
};

/**
 * @brief The level of a pillar of @p years, read over the last window of @p returns.
 *
 * Refused alone for a pillar the sample does not cover, so one short pillar does not cost the
 * rest of the ladder, and for a pillar that is not positive or is longer than the maximum.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<pillar_measurement, std::string>
measure_pillar(std::span<const double> returns, double years, const measurement_settings& settings);

/// Which of a ladder's pillars a sample covers, by the pillar's place in the ladder.
struct pillar_reach final {
    /// The places of the pillars the sample covers, in the order asked for.
    std::vector<std::size_t> reached;
    /// Each pillar the sample does not cover: its place, and the returns its window needs.
    std::vector<std::pair<std::size_t, int>> unreached;
};

/**
 * @brief The pillars of @p pillar_years that a sample of @p returns covers.
 *
 * A pillar is reached when its window holds no more returns than the sample, so a longer sample
 * reaches at least as far. The reach is read from the sample a caller holds and never from a
 * constant, so a source can say which pillars it can quote from the history it has.
 */
[[nodiscard]] ORES_ANALYTICS_QUANT_EXPORT pillar_reach
reach_of(std::span<const double> pillar_years,
         std::size_t returns,
         const measurement_settings& settings);

}

#endif
