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
#ifndef ORES_ANALYTICS_QUANT_MATH_VOL_BATCH_HPP
#define ORES_ANALYTICS_QUANT_MATH_VOL_BATCH_HPP

#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/return_moments.hpp"
#include "ores.analytics.quant/math/volatility_measurement.hpp"
#include <cstdint>
#include <expected>
#include <span>
#include <string>
#include <vector>

namespace ores::analytics::quant::math {

/**
 * @file vol_batch.hpp
 * @brief Many series measured in one pass, in flat arrays, for any asset class.
 *
 * Nothing here names a class. A batch is a set of level histories and one ladder of pillars, and
 * its cells are what the measurement engine reads: the returns a window holds, the level over that
 * window, and the shape of the series' whole sample scaled to the cell's own life. The class
 * supplies the transform, because only the class knows whether a level keeps a sign, and reads the
 * answer to state its own quotes: a rate projects a level onto a swaption, a price carries it to a
 * delta.
 *
 * The shape of the storage is the point. A call per series returning a vector per cell would
 * allocate, branch on a variant and touch a heap for every number, and the platform quotes a whole
 * term structure of every series it holds. So the batch is one array per axis, all of the same
 * length, with an offset per series: reading a row is an offset and not a walk over another row.
 */
enum class vol_cell_state : std::uint8_t {
    /// Allocated and not yet stated. A row a caller leaves unfilled reads as this, so it is
    /// distinguishable from a cell the history does not cover.
    unstated,
    /// The cell holds a level and a shape.
    quoted,
    /// The sample is shorter than the window the pillar needs: no history covers it.
    unreached,
    /// The series or the caller refuses the cell for another cause, which the row states.
    refused
};

/// One series' row of a batch: its cells' slices of every axis, and its own refusals.
struct vol_batch_row final {
    /// The ladder the row's cells answer, in the caller's order.
    std::span<const double> pillars;
    std::span<vol_cell_state> states;
    /// The returns the level was read over: the window's length in returns, or zero.
    std::span<int> window_returns;
    /// The level over the window, in the series' own unit a year, or zero.
    std::span<double> levels;
    /// The skewness of a return over the cell's own life, or zero.
    std::span<double> skewnesses;
    /// The excess kurtosis of a return over the cell's own life, or zero.
    std::span<double> excess_kurtoses;
    /// For each cell, the place of its refusal in =reasons=, or -1 for a quoted cell.
    std::span<std::int32_t> reason_of;
    /// The row's refusal slots: one for the whole series, and one per cell.
    std::span<std::string> reasons;
    /// How many of =reasons= the row has written.
    std::size_t used = 0;

    /// Marks @p cell as @p state with @p text, and records the text's place beside it.
    void refuse(std::size_t cell, vol_cell_state state, std::string text);

    /// Marks every cell as @p state for one cause, sharing one text.
    void refuse_all(vol_cell_state state, std::string text);
};

/**
 * @brief Many series measured at once, over one ladder of pillars.
 *
 * Series @p i owns the cells =[offsets[i], offsets[i + 1])=, and its row is the same slice of every
 * array, so reading one series is an offset and not a walk over another series. Each array is
 * allocated once, exactly, before the pass: the pass allocates nothing per series and nothing per
 * cell.
 */
struct vol_batch final {
    /// The ladder, in the caller's order: the pillars every row's cells answer.
    std::vector<double> pillars;
    /// One more than the number of series: the row of series @p i is =[offsets[i], offsets[i+1])=.
    std::vector<std::size_t> offsets;
    std::vector<vol_cell_state> states;
    std::vector<int> window_returns;
    std::vector<double> levels;
    std::vector<double> skewnesses;
    std::vector<double> excess_kurtoses;
    /// For each cell, the place of its refusal in its row's slots, or -1.
    std::vector<std::int32_t> reason_of;
    /// =pillars + 1= refusal slots per series, one for the series and one per cell.
    std::vector<std::string> reasons;

    /// How many pillars every row answers.
    [[nodiscard]] std::size_t pillar_count() const {
        return pillars.size();
    }

    [[nodiscard]] std::size_t series_count() const {
        return offsets.empty() ? 0 : offsets.size() - 1;
    }

    /// Where @p series' row starts in every axis array.
    [[nodiscard]] std::size_t offset_of(std::size_t series) const {
        return offsets[series];
    }

    /// Where @p series' row starts in =reasons=: one slot per row precedes it.
    [[nodiscard]] std::size_t reason_offset_of(std::size_t series) const {
        return offsets[series] + series;
    }
};

/// The row of @p series, as slices of the batch's own arrays.
ORES_ANALYTICS_QUANT_EXPORT vol_batch_row row_of(vol_batch& batch, std::size_t series);

/**
 * @brief A batch of @p rows series over @p pillars, allocated whole and empty.
 *
 * The one place the layout is decided, so a caller that fills the rows itself -- one task per
 * series, say -- holds the same arrays =measure_batch= would have allocated. Every cell starts
 * =unstated=, which is the state of a cell a caller has not reached: a reader must never see it,
 * and a row that is filled states every cell or refuses it.
 */
ORES_ANALYTICS_QUANT_EXPORT vol_batch make_vol_batch(std::size_t rows,
                                                     std::span<const double> pillars);

/**
 * @brief Fills one series' row, and no other.
 *
 * This is what makes a batch parallel without a lock: every cell a series writes lies in its own
 * row, every read is of the caller's immutable levels and of the settings, and no step keeps a
 * cache between series, so the answer does not depend on the order the rows are filled in.
 *
 * The pass walks the term's returns once, newest first, summing the first four powers of each. A
 * window's moments are then the sums taken so far, so a forty-year window costs what the one before
 * it cost: no window is walked twice, and nothing per cell is allocated. The series' shape is read
 * over every return held, once, and each cell's shape is that shape scaled to the cell's own life.
 *
 * Refused at the batch level only for a caller error: settings no measurement can read, or a row
 * that is not the length of the ladder. A series the history cannot support, and a pillar the
 * sample does not cover, are refusals of their own cells, stated in the row.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<void, std::string>
measure_row(std::span<const dated_level> levels,
            volatility_transform transform,
            const measurement_settings& settings,
            vol_batch_row& row);

/**
 * @brief Every series measured in one batch, over one ladder of pillars.
 *
 * The arithmetic is the one =measure_pillar= applies to a single cell, read in the same order: the
 * window each pillar's length asks for, the sample variance over it, and the shape of the whole
 * sample scaled to the cell's own life. The difference is the shape of the call and of the storage,
 * and the cost: one walk of each series' returns reaches every window of the ladder.
 *
 * A series the history cannot support costs its own row, and a pillar the sample does not cover
 * costs its own cell: the rest of the batch stands, and every cell that is not quoted states why.
 */
ORES_ANALYTICS_QUANT_EXPORT std::expected<vol_batch, std::string>
measure_batch(std::span<const std::span<const dated_level>> series,
              std::span<const double> pillars,
              volatility_transform transform,
              const measurement_settings& settings);

}

#endif
