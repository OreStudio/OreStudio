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
#ifndef ORES_COMPUTE_DOMAIN_NODE_SAMPLE_HPP
#define ORES_COMPUTE_DOMAIN_NODE_SAMPLE_HPP

#include <chrono>
#include <cstdint>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::compute::domain {

/**
 * @brief A single point-in-time sample of per-node execution metrics.
 *
 * Published by ores.compute.wrapper nodes every ~30 seconds via NATS
 * and persisted by ores.compute.service into ores_compute_node_samples_tbl.
 */
struct node_sample final {
    /** @brief When this sample was taken (hypertable partition key). */
    std::chrono::system_clock::time_point sampled_at;

    /** @brief Tenant this node belongs to. */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /** @brief FK to ores_compute_hosts_tbl. */
    boost::uuids::uuid host_id;

    // -------------------------------------------------------------------------
    // Cumulative counters since the wrapper process started
    // -------------------------------------------------------------------------

    int tasks_completed{0};
    int tasks_failed{0};

    // -------------------------------------------------------------------------
    // Delta counters since the previous sample
    // -------------------------------------------------------------------------

    int tasks_since_last{0};

    /** @brief Average task duration for tasks_since_last (milliseconds). */
    std::int64_t avg_task_duration_ms{0};

    /** @brief Peak task duration for tasks_since_last (milliseconds). */
    std::int64_t max_task_duration_ms{0};

    /** @brief Total input bytes fetched for tasks_since_last. */
    std::int64_t input_bytes_fetched{0};

    /** @brief Total output bytes uploaded for tasks_since_last. */
    std::int64_t output_bytes_uploaded{0};

    /** @brief Seconds since the last heartbeat was received for this host. */
    int seconds_since_hb{0};
};

}

#endif
