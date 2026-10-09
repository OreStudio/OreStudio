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
#ifndef ORES_MARKETDATA_CORE_REPOSITORY_CURVE_SNAPSHOT_STALENESS_HPP
#define ORES_MARKETDATA_CORE_REPOSITORY_CURVE_SNAPSHOT_STALENESS_HPP

#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/as_of_rows.hpp"
#include <cstdint>
#include <vector>

namespace ores::marketdata::repository {

/**
 * @brief How far a snapshot's points are from the instant it is read at.
 *
 * Market staleness, not record staleness: the age of the market state a point
 * belongs to, which is what decides whether a drawn curve is one market read or
 * a stitching of several. The spread is the number that measures the mixing --
 * a curve whose points share one instant has a spread of zero however old that
 * instant is, and one stitched across horizons does not.
 */
struct staleness_summary final {
    /**
     * @brief The age of the oldest point of the snapshot, in seconds.
     *
     * Zero for an empty snapshot: it holds no point, so it is in no way mixed.
     */
    std::int64_t oldest_age_seconds = 0;
    std::int64_t spread_seconds = 0;

    /**
     * @brief Whether the snapshot is mixed enough that a view must not draw it
     * silently.
     */
    bool warning = false;
};

/**
 * @brief Summarises the staleness of a snapshot the points are drawn from.
 *
 * A point's instant is the instant it represents; the read's own instant is the
 * one it is relative to, so the two are never taken from two different clocks.
 * A point whose instant is after @p as_of carries no age at all: it neither
 * raises the oldest age nor lowers the newest.
 *
 * A point exactly at @p as_of is age zero and counts, so a snapshot read as of
 * the newest point's own instant still measures the spread from it.
 *
 * @param records The snapshot's points, each with its record time.
 * @param as_of The instant the snapshot is read as of.
 */
ORES_MARKETDATA_CORE_EXPORT staleness_summary summarise_staleness(
    const std::vector<observation_record>& records, std::chrono::system_clock::time_point as_of);

}

#endif
