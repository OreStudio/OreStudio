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
#ifndef ORES_MQ_PGMQ_QUEUE_INFO_HPP
#define ORES_MQ_PGMQ_QUEUE_INFO_HPP

#include <chrono>
#include <string>

namespace ores::mq::pgmq {

/**
 * @brief Metadata about a pgmq queue.
 *
 * Mirrors the pgmq.queue_record composite type returned by pgmq.list_queues().
 */
struct queue_info final {
    /// Name of the queue.
    std::string queue_name;

    /// Timestamp when the queue was created (UTC).
    std::chrono::system_clock::time_point created_at;

    /// True if the queue was created as UNLOGGED (no WAL, faster but non-durable).
    bool is_unlogged{false};

    /// True if the queue is partitioned.
    bool is_partitioned{false};
};

}

#endif
