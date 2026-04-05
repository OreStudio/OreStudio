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
#ifndef ORES_CONTROLLER_API_DOMAIN_SERVICE_DEFINITION_HPP
#define ORES_CONTROLLER_API_DOMAIN_SERVICE_DEFINITION_HPP

#include <string>
#include <chrono>
#include <optional>
#include <boost/uuid/uuid.hpp>

namespace ores::controller::api::domain {

/**
 * @brief Desired state configuration for a managed ORE Studio service.
 *
 * Analogous to a Kubernetes Deployment: describes how many replicas of a
 * service should run, which binary to launch, and what restart policy to
 * apply. The table is bitemporal, so every configuration change is
 * preserved in the historical record.
 */
struct service_definition final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief UUID primary key for the service definition.
     */
    boost::uuids::uuid id = {};

    /**
     * @brief Unique name of the service (e.g. ores.iam.service).
     */
    std::string service_name;

    /**
     * @brief Executable filename (may differ from service_name for
     * multi-instance services).
     */
    std::string binary_name;

    /**
     * @brief Number of replicas to maintain.
     */
    int desired_replicas = 1;

    /**
     * @brief Restart policy: always, on-failure, or never.
     */
    std::string restart_policy{"always"};

    /**
     * @brief Maximum number of automatic restarts before marking the
     * instance as failed.
     */
    int max_restart_count = 3;

    /**
     * @brief 1 if the service should be started, 0 if it should remain
     * stopped.
     */
    int enabled = 1;

    /**
     * @brief Optional command-line argument template. Null means use the
     * default launch arguments.
     */
    std::optional<std::string> args_template;

    /**
     * @brief Human-readable description of the service's role.
     */
    std::optional<std::string> description;

    /**
     * @brief Username of the person who last modified this definition.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     */
    std::chrono::system_clock::time_point recorded_at = {};
};

}

#endif
