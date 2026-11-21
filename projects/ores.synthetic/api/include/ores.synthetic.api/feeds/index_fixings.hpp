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
#ifndef ORES_SYNTHETIC_API_FEEDS_INDEX_FIXINGS_HPP
#define ORES_SYNTHETIC_API_FEEDS_INDEX_FIXINGS_HPP

#include "ores.synthetic.api/export.hpp"
#include <chrono>
#include <cstdint>
#include <expected>
#include <set>
#include <string>
#include <vector>

namespace ores::synthetic::feed {

/**
 * @brief One day of an index's fixing history.
 */
struct index_fixing final {
    std::chrono::year_month_day date;
    /// The rate as decimal text, which is the form the store keeps.
    std::string value;

    friend bool operator==(const index_fixing&, const index_fixing&) = default;
};

/**
 * @brief The oresmd address of the series an index's fixings are published into.
 *
 * An interest rate curve of @p index_family in @p currency at @p tenor states the
 * index of that name: =libor= in GBP at 3M is
 * =oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=3M=. An overnight family
 * has no tenor, so its curve leaves @p tenor empty and its address states none.
 *
 * The address is written by the codec the market reads, so a series a feed creates
 * and a series a trade requires are one text, and not two spellings of one index.
 *
 * @return The address, or why the index has none: an empty currency or family, or
 * an index ORE's own name grammar does not read back.
 */
ORES_SYNTHETIC_API_EXPORT std::expected<std::string, std::string> index_fixing_uri(
    const std::string& currency, const std::string& index_family, const std::string& tenor);

/**
 * @brief How a history's days move away from the level the curve states.
 *
 * The daily spread is in rate terms, so a 2 basis point spread moves a 5% rate and a
 * 0.5% rate by the same 2 basis points. The reversion pulls each day back towards
 * the level, which is what keeps a long history from wandering.
 */
struct fixing_walk final {
    double daily_spread = 0.0002;
    double reversion = 0.01;
};

/**
 * @brief The fixings of an index for the business days ending on @p as_of.
 *
 * The days run from @p as_of back over @p days business days, Monday to Friday. No
 * calendar is read: a fixing on a day a market is shut is a value no leg resets on,
 * because a leg's schedule skips the day, and a publisher that fixes a currency has
 * no calendar the market data store can name.
 *
 * The walk starts at @p level, the rate the curve states for this index at the
 * as-of, and every day's value is the walk's value at that day. The walk is shifted
 * so that the last day, which is @p as_of, is exactly @p level: the history and the
 * curve therefore agree on the day the run starts.
 *
 * The draws come from the run's seed and the index's address, so two runs with one
 * seed state the same history, and a restart republishes the same values. A day in
 * @p held is left out, which is how a value the store already holds, such as a
 * public fixing, is never stated again.
 *
 * @param level The index's rate at @p as_of, as the curve states it.
 * @param as_of The business day the history ends on.
 * @param days How many business days the history covers, counting @p as_of.
 * @param run_seed The run's seed.
 * @param index_uri The index's oresmd address, which keys the draws.
 * @param held The days the series already holds.
 * @param walk The spread and the reversion of the walk.
 *
 * @return The fixings, oldest first, or why none was stated: a level that is not a
 * positive number, or a walk whose spread or reversion is negative.
 */
ORES_SYNTHETIC_API_EXPORT std::expected<std::vector<index_fixing>, std::string>
derive_fixing_history(double level,
                      std::chrono::year_month_day as_of,
                      int days,
                      std::uint64_t run_seed,
                      const std::string& index_uri,
                      const std::set<std::chrono::year_month_day>& held,
                      fixing_walk walk = {});

}

#endif
