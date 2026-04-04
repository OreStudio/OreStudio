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
#ifndef ORES_CONTROLLER_API_MESSAGING_SERVICE_INSTANCE_PROTOCOL_HPP
#define ORES_CONTROLLER_API_MESSAGING_SERVICE_INSTANCE_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <optional>
#include <vector>
#include "ores.controller.api/domain/service_instance.hpp"

namespace ores::controller::api::messaging {

// =============================================================================
// List service instances
// =============================================================================

struct list_service_instances_request {
    using response_type = struct list_service_instances_response;
    static constexpr std::string_view nats_subject =
        "controller.v1.service_instances.list";
    /**
     * @brief Filter by service name. Empty means return all instances.
     */
    std::string service_name;
};

struct list_service_instances_response {
    bool success = false;
    std::string message;
    std::vector<domain::service_instance> service_instances;
};

// =============================================================================
// Start a service (or a specific replica)
// =============================================================================

struct start_service_request {
    using response_type = struct start_service_response;
    static constexpr std::string_view nats_subject =
        "controller.v1.service_instances.start";
    std::string service_name;
    /**
     * @brief Replica index to start. Absent means start all replicas for
     * the service.
     */
    std::optional<int> replica_index;
};

struct start_service_response {
    bool success = false;
    std::string message;
};

// =============================================================================
// Stop a service (or a specific replica)
// =============================================================================

struct stop_service_request {
    using response_type = struct stop_service_response;
    static constexpr std::string_view nats_subject =
        "controller.v1.service_instances.stop";
    std::string service_name;
    /**
     * @brief Replica index to stop. Absent means stop all replicas for
     * the service.
     */
    std::optional<int> replica_index;
};

struct stop_service_response {
    bool success = false;
    std::string message;
};

// =============================================================================
// Restart a service (or a specific replica)
// =============================================================================

struct restart_service_request {
    using response_type = struct restart_service_response;
    static constexpr std::string_view nats_subject =
        "controller.v1.service_instances.restart";
    std::string service_name;
    /**
     * @brief Replica index to restart. Absent means restart all replicas
     * for the service.
     */
    std::optional<int> replica_index;
};

struct restart_service_response {
    bool success = false;
    std::string message;
};

}

#endif
