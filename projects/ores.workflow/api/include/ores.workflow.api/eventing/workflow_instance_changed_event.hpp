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
#ifndef ORES_WORKFLOW_API_EVENTING_WORKFLOW_INSTANCE_CHANGED_EVENT_HPP
#define ORES_WORKFLOW_API_EVENTING_WORKFLOW_INSTANCE_CHANGED_EVENT_HPP

#include <chrono>
#include <string>
#include <vector>
#include "ores.eventing/domain/event_traits.hpp"

namespace ores::workflow::eventing {

/**
 * @brief Domain event indicating that one or more workflow instances
 * have changed state (started, step advanced, completed, failed, or
 * compensation completed).
 *
 * Published by the workflow engine to the NATS event bus whenever a
 * workflow instance transitions state.  Qt clients subscribe via
 * ClientManager::subscribeToEvent and call markAsStale() on receipt to
 * trigger a fresh data load from the query endpoint.
 */
struct workflow_instance_changed_event final {
    /**
     * @brief Timestamp of the state change (UTC).
     */
    std::chrono::system_clock::time_point timestamp;

    /**
     * @brief UUIDs of the workflow instances that changed.
     */
    std::vector<std::string> ids;

    /**
     * @brief Tenant that owns the changed instances.
     */
    std::string tenant_id;
};

}  // namespace ores::workflow::eventing

namespace ores::eventing::domain {

template<>
struct event_traits<ores::workflow::eventing::workflow_instance_changed_event> {
    static constexpr std::string_view name =
        "ores.workflow.workflow_instance_changed";
};

}  // namespace ores::eventing::domain

#endif
