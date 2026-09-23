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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_nats_event_registrar.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.analytics.service/messaging/pricing_engine_type_event_registrar.hpp"
#include "ores.analytics.api/eventing/pricing_engine_type_event.hpp"
#include "ores.analytics.api/messaging/pricing_engine_type_protocol.hpp"
#include "ores.eventing.api/domain/entity_event_traits.hpp"
#include "ores.eventing.core/service/entity_event_publisher.hpp"
#include "ores.eventing.core/service/registrar.hpp"

namespace ores::analytics::service::messaging {

namespace {
namespace ev = ores::eventing;
}

[[nodiscard]] ev::service::subscription
register_pricing_engine_type_event_mapping(ev::service::postgres_event_source& event_source,
                                           ev::service::event_bus& event_bus,
                                           ores::nats::service::client& nats) {
    // The trigger publishes on the table's own channel, and the mapping turns
    // what it says into this entity's event.
    event_source.register_entity_event_mapping<analytics::messaging::pricing_engine_type_event>(
        "ores_analytics_pricing_engine_types");

    return event_bus.subscribe<analytics::messaging::pricing_engine_type_event>(
        [&nats](const analytics::messaging::pricing_engine_type_event& e) {
            // One payload is addressed by three subjects, so the subject is
            // the collection's prefix and the action the event reports.
            ev::service::publish_entity_event(
                nats,
                ev::domain::event_subject<analytics::messaging::pricing_engine_type_event>(
                    e.action),
                e);
        });
}

} // namespace ores::analytics::service::messaging
