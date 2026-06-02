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
#ifndef ORES_COMPUTE_DOMAIN_HOST_HPP
#define ORES_COMPUTE_DOMAIN_HOST_HPP

#include <string>
#include <cstdint>
#include <chrono>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::compute::domain {

/**
 * @brief A registered compute node in the distributed grid.
 *
 * Represents a physical or virtual machine that participates in the BOINC-inspired
 * compute grid. Tracks hardware capabilities, heartbeat, and accumulated credit.
 */
struct host final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID primary key for the host.
     */
    boost::uuids::uuid id;

    /**
     * @brief User-defined name or hostname for the node.
     */
    std::string external_id;

    /**
     * @brief Whimsical adjective+animal display name assigned on first registration.
     * Display-only; never used as a key.
     */
    std::string display_name;

    /**
     * @brief Physical site or region identifier.
     */
    std::string location;

    /**
     * @brief Total logical CPU cores available.
     */
    int cpu_count;

    /**
     * @brief Total system memory in megabytes.
     */
    std::int64_t ram_mb;

    /**
     * @brief GPU model identifier, e.g. 'A100' or NULL if no GPU.
     */
    std::string gpu_type;

    /**
     * @brief Timestamp of the last heartbeat received from this node.
     */
    std::chrono::system_clock::time_point last_rpc_time;

    /**
     * @brief Accumulated work units successfully processed by this host.
     */
    double credit_total;

    /**
     * @brief Username of the person who last modified this compute host.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
