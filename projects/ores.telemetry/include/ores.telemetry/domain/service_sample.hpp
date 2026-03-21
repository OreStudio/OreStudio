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
#ifndef ORES_TELEMETRY_DOMAIN_SERVICE_SAMPLE_HPP
#define ORES_TELEMETRY_DOMAIN_SERVICE_SAMPLE_HPP

#include <chrono>
#include <string>

namespace ores::telemetry::domain {

/**
 * @brief A single heartbeat received from a running domain service.
 *
 * Each domain service embeds a heartbeat_publisher that fires every
 * 15 seconds.  The telemetry service subscribes to the heartbeat subject
 * and persists one row per heartbeat to ores_telemetry_service_samples_tbl.
 *
 * sampled_at is set by the telemetry service at receipt time, so it
 * reflects when the heartbeat arrived rather than when it was sent.
 */
struct service_sample final {
    /** @brief When this heartbeat was received by the telemetry service. */
    std::chrono::system_clock::time_point sampled_at;

    /**
     * @brief Canonical service name (e.g. "ores.compute.service").
     *
     * Matches the name passed to ores::service::service::run().
     */
    std::string service_name;

    /**
     * @brief Per-process UUID, generated once at service startup.
     *
     * Allows distinguishing multiple instances of the same service.
     * Stored as a string in canonical UUID format.
     */
    std::string instance_id;

    /** @brief Service version string (e.g. "1.0"). */
    std::string version;
};

}

#endif
