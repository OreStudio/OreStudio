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
#ifndef ORES_MQ_REPOSITORY_QUEUE_STATS_REPOSITORY_HPP
#define ORES_MQ_REPOSITORY_QUEUE_STATS_REPOSITORY_HPP

#include <chrono>
#include <optional>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.database/domain/context.hpp"
#include "ores.mq/domain/queue_stats.hpp"

namespace ores::mq::repository {

/**
 * @brief Data access layer for queue statistics.
 *
 * Reads from the queue stats tables/views. All methods are stateless.
 */
class queue_stats_repository final {
public:
    using context = ores::database::context;

    /**
     * @brief Returns the latest statistics for all queues.
     */
    std::vector<domain::queue_stats> read_latest(context ctx);

    /**
     * @brief Returns time-series statistics samples for a specific queue.
     *
     * Results are ordered by recorded_at ASC.
     *
     * @param queue_id  The queue to query.
     * @param from      Optional inclusive start of the time window.
     * @param to        Optional inclusive end of the time window.
     */
    std::vector<domain::queue_stats> read_samples(context ctx,
        const boost::uuids::uuid& queue_id,
        std::optional<std::chrono::system_clock::time_point> from = {},
        std::optional<std::chrono::system_clock::time_point> to = {});
};

}

#endif
