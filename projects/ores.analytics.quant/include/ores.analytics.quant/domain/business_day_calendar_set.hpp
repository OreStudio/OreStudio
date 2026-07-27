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
#ifndef ORES_ANALYTICS_QUANT_DOMAIN_BUSINESS_DAY_CALENDAR_SET_HPP
#define ORES_ANALYTICS_QUANT_DOMAIN_BUSINESS_DAY_CALENDAR_SET_HPP

#include "ores.analytics.quant/export.hpp"
#include <bitset>
#include <chrono>
#include <cstddef>
#include <span>
#include <vector>

namespace ores::analytics::quant::domain {

/// One materialised holiday row, the shape a bulk
/// @c SELECT ... ORDER BY calendar_code, date returns -- @c from_rows
/// requires rows for the same calendar to be contiguous and sorted by date,
/// exactly what that query gives for free.
struct calendar_date_row final {
    std::size_t calendar_index;
    std::chrono::year_month_day date;
};

/// A single "is this date a business day for this calendar" question, keyed
/// by index into the calendar set rather than by code -- the caller owns the
/// code<->index mapping (see the refdata-coupling boundary this component
/// keeps to).
struct calendar_query final {
    std::size_t calendar_index;
    std::chrono::year_month_day date;
};

/// Bulk-built, read-only view over materialised holiday dates for many
/// calendars at once, laid out CSR-style (one flat, sorted holiday array
/// plus per-calendar offsets) to match how the data actually arrives -- a
/// single ordered SELECT -- instead of N separately heap-allocated
/// std::vector<year_month_day> per calendar.
class ORES_ANALYTICS_QUANT_EXPORT business_day_calendar_set final {
public:
    /// @p rows must be ordered by (calendar_index, date), duplicates
    /// removed by the caller (or -- since holiday sets are typically small
    /// and rows come from a DISTINCT query -- absent by construction).
    /// @p calendar_count must be >= the highest calendar_index in @p rows.
    [[nodiscard]] static business_day_calendar_set
    from_rows(std::span<const calendar_date_row> rows,
              std::size_t calendar_count,
              std::vector<std::bitset<7>> weekend_masks);

    /// Answers each query against the flat holiday storage via binary
    /// search, aligned 1:1 with @p queries by index.
    [[nodiscard]] std::vector<bool>
    is_business_day_batch(std::span<const calendar_query> queries) const;

    [[nodiscard]] std::size_t calendar_count() const noexcept {
        return calendar_offsets_.empty() ? 0 : calendar_offsets_.size() - 1;
    }

    [[nodiscard]] std::size_t holiday_count(std::size_t calendar_index) const noexcept {
        if (calendar_index + 1 >= calendar_offsets_.size())
            return 0;
        return calendar_offsets_[calendar_index + 1] - calendar_offsets_[calendar_index];
    }

private:
    business_day_calendar_set(std::vector<std::chrono::year_month_day> holidays,
                              std::vector<std::size_t> calendar_offsets,
                              std::vector<std::bitset<7>> weekend_masks)
        : holidays_(std::move(holidays))
        , calendar_offsets_(std::move(calendar_offsets))
        , weekend_masks_(std::move(weekend_masks)) {}

    std::vector<std::chrono::year_month_day> holidays_; // flat, sorted per-calendar segment
    std::vector<std::size_t> calendar_offsets_; // CSR row-pointers, size = calendar_count + 1
    std::vector<std::bitset<7>> weekend_masks_; // per calendar, indexed by calendar_index
};

} // namespace ores::analytics::quant::domain

#endif
