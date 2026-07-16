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
#include "ores.dq.service/messaging/badge_severity_event_registrar.hpp"
#include "ores.dq.api/eventing/badge_severity_changed_event.hpp"
#include "ores.eventing.api/domain/entity_change_event.hpp"
#include "ores.eventing.core/service/entity_event_publisher.hpp"
#include "ores.eventing.core/service/registrar.hpp"

namespace ores::dq::service::messaging {

namespace {
namespace ev = ores::eventing;
}

[[nodiscard]] ev::service::subscription
register_badge_severity_event_mapping(ev::service::postgres_event_source& event_source,
                                      ev::service::event_bus& event_bus,
                                      ores::nats::service::client& nats) {
    ev::service::registrar::register_mapping<dq::eventing::badge_severity_changed_event>(
        event_source, "ores.dq.badge_severity", "ores_dq_badge_severities");

    return event_bus.subscribe<dq::eventing::badge_severity_changed_event>(
        [&nats](const dq::eventing::badge_severity_changed_event& e) {
            ev::service::publish_entity_event(
                nats,
                std::string(
                    ev::domain::event_traits<dq::eventing::badge_severity_changed_event>::name),
                ev::domain::entity_change_event{.entity = "ores.dq.badge_severity",
                                                .timestamp = e.timestamp,
                                                .entity_ids = e.codes,
                                                .tenant_id = e.tenant_id});
        });
}

} // namespace ores::dq::service::messaging
