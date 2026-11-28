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
#ifndef ORES_ANALYTICS_QUANT_TESTS_VOL_TEST_DATA_HPP
#define ORES_ANALYTICS_QUANT_TESTS_VOL_TEST_DATA_HPP

#include "ores.analytics.quant/math/ir_vol_derivation.hpp"
#include "ores.analytics.quant/math/volatility_measurement.hpp"
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <random>
#include <span>
#include <vector>

/**
 * @file vol_test_data.hpp
 * @brief The histories the derivation suites measure, generated the same way for each.
 *
 * A batch and the projection over it are two views of one set of series, so the suites that test
 * them must read the same series. The generator lives here rather than in either suite, and its
 * seeds keep each case's series apart.
 */
namespace ores::analytics::quant::tests {

/// A day the derivation suites date their histories on, clear of the other suites' helpers.
inline std::chrono::year_month_day vol_observation_day(int day) {
    using std::chrono::January;
    using std::chrono::sys_days;
    using std::chrono::year;
    return std::chrono::year_month_day{sys_days{year{2026} / January / day}};
}

/**
 * @brief A rate that wanders from four percent, one observation a day, on a seed of its own.
 *
 * A derivation runs over many series at once, so each series needs a history that is plausible and
 * reproducible: a random walk of a few basis points a day, held inside a band.
 */
inline std::vector<math::dated_level> vol_wandering_rate(int count, std::uint32_t seed) {
    std::mt19937 generator(seed);
    std::normal_distribution<double> step(0.0, 0.0002);
    std::vector<math::dated_level> out;
    out.reserve(static_cast<std::size_t>(count));
    double rate = 0.04;
    for (int i = 0; i < count; ++i) {
        rate = std::clamp(rate + step(generator), 0.005, 0.12);
        out.push_back({.date = vol_observation_day(i + 1), .level = rate});
    }
    return out;
}

/// A set of histories as a batch takes them: levels and a ladder, and no other attribute.
inline std::vector<std::span<const math::dated_level>>
vol_histories_of(const std::vector<std::vector<math::dated_level>>& levels) {
    std::vector<std::span<const math::dated_level>> out;
    out.reserve(levels.size());
    for (const auto& history : levels)
        out.emplace_back(history);
    return out;
}

/// The projection the swaption suites read through.
inline math::short_rate_projection vol_reversion(double mean_reversion) {
    return {.model = math::short_rate_model::hull_white, .mean_reversion = mean_reversion};
}

}

#endif
