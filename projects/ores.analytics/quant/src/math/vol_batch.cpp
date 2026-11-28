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
#include "ores.analytics.quant/math/vol_batch.hpp"
#include <algorithm>
#include <cmath>
#include <format>
#include <optional>
#include <utility>

namespace ores::analytics::quant::math {

namespace {

/// Whether a level of @p transform's series is an observation, as returns_of reads it.
bool is_an_observation(double level, volatility_transform transform) {
    if (!std::isfinite(level))
        return false;
    return transform == volatility_transform::difference || level > 0.0;
}

/// One cell of the ladder, in the order the pass reaches its window.
struct ladder_cell final {
    std::size_t cell;
    int window;
};

/**
 * The ladder as the pass reaches it: one entry per pillar that can be measured, ordered by the
 * window it reads, so a single walk of the returns meets each window once.
 *
 * The order is the batch's, not the series': a window is a function of the pillar and of the
 * settings, so every series of the batch walks the same list.
 */
std::vector<ladder_cell> ladder_of(std::span<const double> pillars,
                                   const measurement_settings& settings) {
    std::vector<ladder_cell> out;
    out.reserve(pillars.size());
    for (std::size_t cell = 0; cell < pillars.size(); ++cell) {
        const double years = pillars[cell];
        if (years > 0.0 && years <= maximum_pillar_years)
            out.push_back({.cell = cell, .window = measurement_window(years, settings)});
    }
    std::ranges::stable_sort(out, {}, [](const auto& cell) { return cell.window; });
    return out;
}

/// The return of @p value from @p previous, by @p transform, both already observable.
double return_of(double value, double previous, volatility_transform transform) {
    return transform == volatility_transform::log_return ? std::log(value) - std::log(previous) :
                                                           value - previous;
}

}

void vol_batch_row::refuse(std::size_t cell, vol_cell_state state, std::string text) {
    states[cell] = state;
    reasons[used] = std::move(text);
    reason_of[cell] = static_cast<std::int32_t>(used);
    windows[cell] = 0;
    levels[cell] = 0.0;
    skewnesses[cell] = 0.0;
    excess_kurtoses[cell] = 0.0;
    ++used;
}

void vol_batch_row::refuse_all(vol_cell_state state, std::string text) {
    reasons[used] = std::move(text);
    const auto place = static_cast<std::int32_t>(used);
    ++used;
    for (std::size_t cell = 0; cell < states.size(); ++cell) {
        states[cell] = state;
        reason_of[cell] = place;
        windows[cell] = 0;
        levels[cell] = 0.0;
        skewnesses[cell] = 0.0;
        excess_kurtoses[cell] = 0.0;
    }
}

vol_batch_row row_of(vol_batch& batch, std::size_t series) {
    const std::size_t first = batch.offsets[series];
    const std::size_t cells = batch.offsets[series + 1] - first;
    const std::size_t slot = batch.reason_offset_of(series);
    return vol_batch_row{.pillars = batch.pillars,
                         .states = std::span(batch.states).subspan(first, cells),
                         .windows = std::span(batch.windows).subspan(first, cells),
                         .levels = std::span(batch.levels).subspan(first, cells),
                         .skewnesses = std::span(batch.skewnesses).subspan(first, cells),
                         .excess_kurtoses = std::span(batch.excess_kurtoses).subspan(first, cells),
                         .reason_of = std::span(batch.reason_of).subspan(first, cells),
                         .reasons =
                             std::span(batch.reasons).subspan(slot, batch.pillars.size() + 1),
                         .used = 0};
}

vol_batch make_vol_batch(std::size_t rows, std::span<const double> pillars) {
    vol_batch out;
    out.pillars.assign(pillars.begin(), pillars.end());
    const std::size_t cells = rows * pillars.size();

    out.offsets.resize(rows + 1);
    for (std::size_t row = 0; row <= rows; ++row)
        out.offsets[row] = row * pillars.size();
    out.states.assign(cells, vol_cell_state::unreached);
    out.windows.assign(cells, 0);
    out.levels.assign(cells, 0.0);
    out.skewnesses.assign(cells, 0.0);
    out.excess_kurtoses.assign(cells, 0.0);
    out.reason_of.assign(cells, -1);
    out.reasons.resize(rows * (pillars.size() + 1));
    return out;
}

std::expected<void, std::string> measure_row(std::span<const dated_level> levels,
                                             volatility_transform transform,
                                             const measurement_settings& settings,
                                             vol_batch_row& row) {
    if (!(settings.periods_per_year > 0.0) || !std::isfinite(settings.periods_per_year))
        return std::unexpected("the periods in a year must be positive and finite");
    const std::size_t cells = row.pillars.size();
    if (row.states.size() != cells || row.levels.size() != cells || row.windows.size() != cells ||
        row.skewnesses.size() != cells || row.excess_kurtoses.size() != cells ||
        row.reason_of.size() != cells)
        return std::unexpected(
            std::format("the row does not hold the {} cell(s) of the ladder", cells));
    if (row.reasons.size() < cells + 1)
        return std::unexpected(std::format(
            "the row holds {} refusal slot(s) for {} cell(s)", row.reasons.size(), cells));

    // A pillar no measurement can read is refused before the history is walked, and the walk
    // never looks for its window.
    for (std::size_t cell = 0; cell < cells; ++cell)
        if (!(row.pillars[cell] > 0.0) || !(row.pillars[cell] <= maximum_pillar_years))
            row.refuse(cell,
                       vol_cell_state::refused,
                       std::format("the pillar must be positive and at most {} years",
                                   static_cast<int>(maximum_pillar_years)));

    const auto ladder = ladder_of(row.pillars, settings);

    // The returns are taken from the newest level backwards so the sums of a window are the sums
    // since the end: one walk reaches every window, and each window costs only the returns the one
    // before it did not.
    std::optional<double> newer;
    int count = 0;
    double sum = 0.0;
    double squares = 0.0;
    double cubes = 0.0;
    double fourths = 0.0;
    std::size_t next = 0;
    for (auto level = levels.rbegin(); level != levels.rend(); ++level) {
        if (!is_an_observation(level->level, transform))
            continue;
        if (!newer) {
            newer = level->level;
            continue;
        }
        const double x = return_of(*newer, level->level, transform);
        newer = level->level;
        const double x2 = x * x;
        sum += x;
        squares += x2;
        cubes += x2 * x;
        fourths += x2 * x2;
        ++count;
        while (next < ladder.size() && ladder[next].window == count) {
            const std::size_t cell = ladder[next].cell;
            const double n = count;
            const double mean = sum / n;
            // The sample variance of the window, demeaning as the engine does, and held at zero
            // where a flat window leaves only rounding below it.
            const double variance = std::max((squares / n - mean * mean) * n / (n - 1.0), 0.0);
            row.windows[cell] = count;
            row.levels[cell] =
                std::sqrt(variance * settings.periods_per_year) * settings.level_multiplier;
            row.states[cell] = vol_cell_state::quoted;
            ++next;
        }
    }

    if (count < 2) {
        row.refuse_all(vol_cell_state::refused,
                       "the series has no return: it holds fewer than two observations");
        return {};
    }

    // The shape is the whole sample's, and it is read once: it does not depend on the pillar's
    // window, only on the series and on the life the cell's return covers.
    const auto sample = moments_from_power_sums(count, sum, squares, cubes, fourths);
    if (!sample) {
        row.refuse_all(vol_cell_state::refused, sample.error());
        return {};
    }
    const auto shape = shrink(*sample, settings.prior);

    for (const auto& cell : ladder) {
        if (row.states[cell.cell] != vol_cell_state::quoted) {
            // The walk met every window the sample could cover, so a cell still unquoted is one
            // the sample is too short for.
            row.refuse(cell.cell,
                       vol_cell_state::unreached,
                       std::format("the pillar of {} years needs {} returns; the sample holds {}",
                                   row.pillars[cell.cell],
                                   cell.window,
                                   count));
            continue;
        }
        const auto scaled = over_days(shape, row.pillars[cell.cell] * settings.periods_per_year);
        row.skewnesses[cell.cell] = scaled.skewness;
        row.excess_kurtoses[cell.cell] = scaled.excess_kurtosis;
    }
    return {};
}

std::expected<vol_batch, std::string>
measure_batch(std::span<const std::span<const dated_level>> series,
              std::span<const double> pillars,
              volatility_transform transform,
              const measurement_settings& settings) {
    if (!(settings.periods_per_year > 0.0) || !std::isfinite(settings.periods_per_year))
        return std::unexpected("the periods in a year must be positive and finite");

    auto out = make_vol_batch(series.size(), pillars);
    for (std::size_t row = 0; row < series.size(); ++row) {
        auto slice = row_of(out, row);
        const auto measured = measure_row(series[row], transform, settings, slice);
        if (!measured)
            return std::unexpected(measured.error());
    }
    return out;
}

}
