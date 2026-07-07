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
#ifndef ORES_EVENTING_CORE_SERVICE_ENTITY_EVENT_PUBLISHER_HPP
#define ORES_EVENTING_CORE_SERVICE_ENTITY_EVENT_PUBLISHER_HPP

#include "ores.eventing.api/domain/entity_change_event.hpp"
#include "ores.eventing.core/export.hpp"
#include "ores.nats/service/client.hpp"
#include <string>

namespace ores::eventing::service {

/**
 * @brief Publishes an entity_change_event to NATS on the given subject.
 *
 * Serializes the notification to JSON and publishes it, logging (rather than
 * throwing) on failure. Shared by every component's per-entity event-mapping
 * registration so the publish/error-handling logic is defined once.
 */
ORES_EVENTING_CORE_EXPORT void
publish_entity_event(ores::nats::service::client& nats,
                     const std::string& subject,
                     const domain::entity_change_event& notification);

}

#endif
