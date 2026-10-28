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
#ifndef ORES_MARKETDATA_API_DOMAIN_FEED_START_HPP
#define ORES_MARKETDATA_API_DOMAIN_FEED_START_HPP

#include <chrono>
#include <optional>
#include <string>

namespace ores::marketdata::domain {

/**
 * @brief Where a feed's starting value came from, or that it kept its fixed level.
 *
 * A feed that starts from a published value records the one it took: the item, the
 * provider, the provider's own id of the item, the date of the value and the time it was
 * received. A feed with no published value to take keeps its configured level, and says
 * why in @c note. The record belongs to the run, not to the config: a feed that stops and
 * starts again records where the new run started.
 */
struct feed_start final {
    /// True when a fixing set the level; false when the feed kept its fixed level.
    bool from_fixing = false;
    /// The fixing's item, as the oresmd URI that names it.
    std::string item;
    /// The provider that published the fixing.
    std::string provider_code;
    /// The provider's own id of the item, or empty when no mapping names it.
    std::string vendor_id;
    /// True when the item quotes the feed's pair the other way round.
    bool inverted = false;
    /// The date of the fixing, which is the publisher's own time for a daily fixing.
    std::optional<std::chrono::year_month_day> fixing_date;
    /// When the service stored the fixing.
    std::optional<std::chrono::system_clock::time_point> received_at;
    /// The level the feed started from, read the feed's pair the way round it quotes it.
    double level = 0.0;
    /// How many calendar days of fixings the window covered.
    int window_days = 0;
    /// How many fixings of the window measured the spread.
    int used = 0;
    /// The measured hourly spread, when the window had one.
    std::optional<double> hourly_spread;
    /// Why the feed kept its fixed level, or why its spread was not measured.
    std::string note;
};

}

#endif
