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
#ifndef ORES_MQ_PGMQ_QUEUE_METRICS_HPP
#define ORES_MQ_PGMQ_QUEUE_METRICS_HPP

#include <chrono>
#include <cstdint>
#include <optional>
#include <string>

namespace ores::mq::pgmq {

/**
 * @brief Statistics about a pgmq queue.
 *
 * Mirrors the pgmq.metrics_result composite type returned by pgmq.metrics()
 * and pgmq.metrics_all().
 */
struct queue_metrics final {
    /// Name of the queue.
    std::string queue_name;

    /// Number of messages currently visible in the queue.
    int64_t queue_length{0};

    /// Age in seconds of the newest message (nullopt if queue is empty).
    std::optional<int32_t> newest_msg_age_sec;

    /// Age in seconds of the oldest message (nullopt if queue is empty).
    std::optional<int32_t> oldest_msg_age_sec;

    /// Total number of messages ever sent to this queue.
    int64_t total_messages{0};

    /// Timestamp when these metrics were collected (UTC).
    std::chrono::system_clock::time_point scrape_time;
};

}

#endif
