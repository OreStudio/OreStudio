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
#ifndef ORES_CONTROLLER_API_DOMAIN_SERVICE_EVENT_HPP
#define ORES_CONTROLLER_API_DOMAIN_SERVICE_EVENT_HPP

#include <string>
#include <chrono>
#include <optional>
#include <boost/uuid/uuid.hpp>

namespace ores::controller::api::domain {

/**
 * @brief A lifecycle event for a managed service instance.
 *
 * Analogous to a Kubernetes Event: one immutable record of a significant
 * state transition (started, stopped, crashed, restarted, etc.). Stored in
 * a TimescaleDB hypertable partitioned by occurred_at for efficient
 * time-range queries and automatic retention.
 */
struct service_event final {
    /**
     * @brief Timestamp when the event occurred (also the hypertable
     * partition key).
     */
    std::chrono::system_clock::time_point occurred_at = {};

    /**
     * @brief UUID identifying this specific event.
     */
    boost::uuids::uuid event_id = {};

    /**
     * @brief Name of the service this event relates to.
     */
    std::string service_name;

    /**
     * @brief Instance UUID, if the event is tied to a specific replica.
     * Absent for service-level events (e.g. definition changes).
     */
    std::optional<boost::uuids::uuid> instance_id;

    /**
     * @brief Replica index, if the event is tied to a specific replica.
     */
    std::optional<int> replica_index;

    /**
     * @brief Short event type code (e.g. started, stopped, crashed,
     * restarted, definition_updated).
     */
    std::string event_type;

    /**
     * @brief Human-readable message providing additional context.
     */
    std::string message;
};

}

#endif
