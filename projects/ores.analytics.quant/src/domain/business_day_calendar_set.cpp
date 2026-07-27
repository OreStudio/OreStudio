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
#include "ores.analytics.quant/domain/business_day_calendar_set.hpp"
#include <algorithm>

namespace ores::analytics::quant::domain {

business_day_calendar_set
business_day_calendar_set::from_rows(std::span<const calendar_date_row> rows,
                                     std::size_t calendar_count,
                                     std::vector<std::bitset<7>> weekend_masks) {
    weekend_masks.resize(calendar_count);

    std::vector<std::chrono::year_month_day> holidays;
    holidays.reserve(rows.size());
    std::vector<std::size_t> offsets(calendar_count + 1, 0);

    for (const auto& row : rows) {
        holidays.push_back(row.date);
        if (row.calendar_index + 1 < offsets.size())
            ++offsets[row.calendar_index + 1];
    }
    for (std::size_t i = 1; i < offsets.size(); ++i)
        offsets[i] += offsets[i - 1];

    return business_day_calendar_set{
        std::move(holidays), std::move(offsets), std::move(weekend_masks)};
}

std::vector<bool>
business_day_calendar_set::is_business_day_batch(std::span<const calendar_query> queries) const {
    std::vector<bool> result;
    result.reserve(queries.size());

    for (const auto& q : queries) {
        bool is_business_day = true;

        if (q.calendar_index < weekend_masks_.size()) {
            const std::chrono::sys_days d{q.date};
            const std::chrono::weekday w{d};
            if (weekend_masks_[q.calendar_index].test(w.c_encoding()))
                is_business_day = false;
        }

        if (is_business_day && q.calendar_index + 1 < calendar_offsets_.size()) {
            const auto begin = holidays_.cbegin() +
                               static_cast<std::ptrdiff_t>(calendar_offsets_[q.calendar_index]);
            const auto end = holidays_.cbegin() +
                             static_cast<std::ptrdiff_t>(calendar_offsets_[q.calendar_index + 1]);
            if (std::binary_search(begin, end, q.date))
                is_business_day = false;
        }

        result.push_back(is_business_day);
    }

    return result;
}

} // namespace ores::analytics::quant::domain
