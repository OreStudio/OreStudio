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
#ifndef ORES_CONTROLLER_API_MESSAGING_SERVICE_EVENT_PROTOCOL_HPP
#define ORES_CONTROLLER_API_MESSAGING_SERVICE_EVENT_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <cstdint>
#include <vector>
#include "ores.controller.api/domain/service_event.hpp"

namespace ores::controller::api::messaging {

// =============================================================================
// List service lifecycle events
// =============================================================================

struct list_service_events_request {
    using response_type = struct list_service_events_response;
    static constexpr std::string_view nats_subject =
        "controller.v1.service_events.list";
    /**
     * @brief Filter by service name. Empty means return events for all
     * services.
     */
    std::string service_name;
    /**
     * @brief Maximum number of events to return (most recent first).
     */
    int limit = 100;
};

struct list_service_events_response {
    bool success = false;
    std::string message;
    std::vector<domain::service_event> service_events;
    std::uint64_t total_count = 0;
};

}

#endif
