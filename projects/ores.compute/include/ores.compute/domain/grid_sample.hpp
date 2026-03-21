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
#ifndef ORES_COMPUTE_DOMAIN_GRID_SAMPLE_HPP
#define ORES_COMPUTE_DOMAIN_GRID_SAMPLE_HPP

#include <chrono>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::compute::domain {

/**
 * @brief A single point-in-time snapshot of server-side compute grid metrics.
 *
 * Written by the compute_grid_poller in ores.compute.service every
 * ~30 seconds.  One row per sample interval per tenant in
 * ores_compute_grid_samples_tbl.
 */
struct grid_sample final {
    /** @brief When this sample was taken (hypertable partition key). */
    std::chrono::system_clock::time_point sampled_at;

    /** @brief Tenant this sample belongs to. */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    // -------------------------------------------------------------------------
    // Host counts
    // -------------------------------------------------------------------------

    /** @brief Total registered hosts (all versions). */
    int total_hosts{0};

    /**
     * @brief Hosts whose last heartbeat is within the 5-minute reaper window.
     */
    int online_hosts{0};

    /**
     * @brief Online hosts that have no InProgress result assigned to them.
     */
    int idle_hosts{0};

    // -------------------------------------------------------------------------
    // Result state breakdown  (server_state codes)
    // -------------------------------------------------------------------------

    /** @brief Results in state 1 (Inactive). */
    int results_inactive{0};

    /** @brief Results in state 2 (Unsent). */
    int results_unsent{0};

    /** @brief Results in state 4 (InProgress). */
    int results_in_progress{0};

    /** @brief Results in state 5 (Done). */
    int results_done{0};

    // -------------------------------------------------------------------------
    // Workunit / batch counts
    // -------------------------------------------------------------------------

    /** @brief Total workunits. */
    int total_workunits{0};

    /** @brief Total batches. */
    int total_batches{0};

    /**
     * @brief Batches that have at least one InProgress result.
     */
    int active_batches{0};

    // -------------------------------------------------------------------------
    // Outcome breakdown for results completed in the last 24 hours
    // (outcome codes: 1=Success 3=ClientError 4=NoReply)
    // -------------------------------------------------------------------------

    int outcomes_success{0};
    int outcomes_client_error{0};
    int outcomes_no_reply{0};
};

}

#endif
