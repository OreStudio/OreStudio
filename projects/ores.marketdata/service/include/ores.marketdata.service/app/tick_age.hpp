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
#ifndef ORES_MARKETDATA_SERVICE_APP_TICK_AGE_HPP
#define ORES_MARKETDATA_SERVICE_APP_TICK_AGE_HPP

#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include <algorithm>
#include <chrono>
#include <cstddef>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief Removes the ticks of @p ticks that are older than @p max_age at @p now.
 *
 * A tick is perishable. A tick that waits while a newer one arrives is worse than a
 * tick that is lost, so a worker that has fallen behind drops what has aged out and
 * does not work through it. The age is @p now minus the observation time of the
 * tick. A tick stamped in the future is kept. The ticks that stay keep their order.
 *
 * @return The number of ticks removed.
 */
inline std::size_t erase_stale(std::vector<messaging::market_tick>& ticks,
                               std::chrono::system_clock::time_point now,
                               std::chrono::milliseconds max_age) {
    const auto stale = [&](const messaging::market_tick& t) {
        return now - t.observation_time > max_age;
    };
    const auto removed = std::erase_if(ticks, stale);
    return removed;
}

}

#endif
