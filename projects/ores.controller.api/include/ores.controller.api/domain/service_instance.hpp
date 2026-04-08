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
#ifndef ORES_CONTROLLER_API_DOMAIN_SERVICE_INSTANCE_HPP
#define ORES_CONTROLLER_API_DOMAIN_SERVICE_INSTANCE_HPP

#include <string>
#include <chrono>
#include <optional>
#include <boost/uuid/uuid.hpp>

namespace ores::controller::api::domain {

/**
 * @brief Operational state of a single running replica of a managed service.
 *
 * Analogous to a Kubernetes Pod: one concrete running instance of a service
 * definition. Tracks the OS process ID, lifecycle phase, start/stop times,
 * and how many times the instance has been automatically restarted. This is
 * a non-temporal table — only current state is kept here; history is captured
 * in service_event records.
 */
struct service_instance final {
    /**
     * @brief UUID primary key for the instance.
     */
    boost::uuids::uuid id = {};

    /**
     * @brief Name of the service definition this instance belongs to.
     */
    std::string service_name;

    /**
     * @brief Zero-based replica number; used to generate unique log file
     * names (e.g. ores.iam.service.0.log).
     */
    int replica_index = 0;

    /**
     * @brief OS process ID while running; absent when stopped or pending.
     */
    std::optional<int> pid;

    /**
     * @brief Lifecycle phase: pending, running, stopping, stopped, failed.
     */
    std::string phase{"pending"};

    /**
     * @brief Timestamp when the process was last successfully started.
     */
    std::optional<std::chrono::system_clock::time_point> started_at;

    /**
     * @brief Timestamp when the process last stopped or was stopped.
     */
    std::optional<std::chrono::system_clock::time_point> stopped_at;

    /**
     * @brief Number of times this instance has been automatically restarted.
     */
    int restart_count = 0;

    /**
     * @brief Human-readable reason for the most recent failure, if any.
     *
     * Set by the controller when phase transitions to "failed" (max_restart_count
     * exceeded). Includes the interpreted exit code and a snippet from the log.
     * Absent for instances that have never failed.
     */
    std::optional<std::string> last_error;

    /**
     * @brief Timestamp when this row was first created.
     */
    std::chrono::system_clock::time_point created_at = {};
};

}

#endif
