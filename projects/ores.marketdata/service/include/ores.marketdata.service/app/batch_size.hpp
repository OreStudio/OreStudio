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
#ifndef ORES_MARKETDATA_SERVICE_APP_BATCH_SIZE_HPP
#define ORES_MARKETDATA_SERVICE_APP_BATCH_SIZE_HPP

#include <algorithm>
#include <chrono>
#include <cstddef>
#include <cstdint>

namespace ores::marketdata::service::app {

/**
 * @brief The number of ticks the next batch should hold, so that it takes about @p target_ms.
 *
 * A batch of @p last_ticks took @p took_ms. The next batch holds as many ticks as the same
 * speed gives in @p target_ms, between @p lowest and @p highest. A store that is slow gets
 * smaller batches, so a tick waits less inside a batch, and a fast store gets larger ones
 * that share the cost of a copy among more ticks.
 */
[[nodiscard]] inline std::size_t next_batch_ticks(std::size_t last_ticks,
                                                  std::int64_t took_ms,
                                                  std::int64_t target_ms,
                                                  std::size_t lowest,
                                                  std::size_t highest) {
    const auto took = std::max<std::int64_t>(1, took_ms);
    const auto sized = static_cast<std::int64_t>(last_ticks) * target_ms / took;
    return static_cast<std::size_t>(std::clamp<std::int64_t>(
        sized, static_cast<std::int64_t>(lowest), static_cast<std::int64_t>(highest)));
}

}

#endif
