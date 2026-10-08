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
#include "ores.marketdata.core/repository/curve_snapshot_staleness.hpp"
#include <algorithm>
#include <chrono>

namespace ores::marketdata::repository {

namespace {

/*
 * The spread at which a snapshot stops being one market state. The journey
 * states the threshold; this is its enforcement. Half an hour is inside the
 * publication lag of the slowest feed the curves are stitched from, so a spread
 * at or above it means at least two publication rounds met in one plot.
 */
constexpr std::int64_t mixing_threshold_seconds = 30 * 60;

}

staleness_summary summarise_staleness(const std::vector<observation_record>& records,
                                      std::chrono::system_clock::time_point as_of) {
    staleness_summary summary;
    if (records.empty())
        return summary;

    std::int64_t oldest = 0;
    std::int64_t newest = 0;
    bool any = false;
    for (const auto& record : records) {
        const auto age = std::chrono::duration_cast<std::chrono::seconds>(
                             as_of - record.observation.observation_datetime)
                             .count();
        if (age < 0)
            continue;
        oldest = std::max(oldest, age);
        newest = any ? std::min(newest, age) : age;
        any = true;
    }

    summary.oldest_age_seconds = oldest;
    summary.spread_seconds = oldest - newest;
    summary.warning = summary.spread_seconds >= mixing_threshold_seconds;
    return summary;
}

}
