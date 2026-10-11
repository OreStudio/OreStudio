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
#include "ores.eventing.api/domain/entity_event_traits.hpp"
#include "ores.eventing.core/export.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/domain/headers.hpp"
#include <optional>
#include <stdexcept>
#include <string>
#include <unordered_map>

namespace ores::eventing::service {

/**
 * @brief Publishes an entity_change_event to NATS on the given subject.
 *
 * Serializes the notification to JSON and publishes it. On failure it
 * rethrows with the subject in the message: every call site is an event_bus
 * subscriber callback, and the bus catches handler exceptions, logs them at
 * error, and reports the partial delivery in its summary. A failed publish
 * is therefore never silent. Shared by every component's per-entity
 * event-mapping registration so the publish/error-handling logic is defined
 * once.
 */
ORES_EVENTING_CORE_EXPORT void
publish_entity_event(ores::nats::service::client& nats,
                     const std::string& subject,
                     const domain::entity_change_event& notification);

/**
 * @brief The envelope headers an entity event is published with.
 *
 * The tenant is always present; the party only when the row has one. An event
 * without the party header concerns the whole tenant. A tenant that is unknown
 * is left out rather than sent empty, because a subscriber treats a missing
 * tenant as an event it cannot show to anyone.
 */
[[nodiscard]] inline std::unordered_map<std::string, std::string>
entity_event_headers(const std::string& tenant_id, const std::optional<std::string>& party_id) {
    std::unordered_map<std::string, std::string> headers;
    if (!tenant_id.empty())
        headers.emplace(std::string(ores::nats::headers::x_tenant_id), tenant_id);
    if (party_id && !party_id->empty())
        headers.emplace(std::string(ores::nats::headers::x_party_id), *party_id);
    return headers;
}

/**
 * @brief Publishes a canonical entity event to NATS on the given subject.
 *
 * The event is published as the event type states it, so the key travels as
 * the entity's own key record rather than as a list of opaque identifiers.
 * The subject is the event's collection prefix plus the action the event
 * reports, which is what
ef event_subject states.
 *
 * On failure it rethrows with the subject in the message, for the same reason
 * the change-event overload does: every call site is an event_bus subscriber
 * callback, and the bus reports a failed handler rather than swallowing it.
 *
 * @param nats The client to publish with.
 * @param subject The event subject, from
ef event_subject.
 * @param event The typed event to publish.
 */
template <typename Event>
void publish_entity_event(ores::nats::service::client& nats,
                          const std::string& subject,
                          const Event& event) {
    try {
        nats.publish(subject, ores::nats::default_wire_codec().encode(event), {});
    } catch (const std::exception& e) {
        throw std::runtime_error("Failed to publish to NATS subject '" + subject +
                                 "': " + e.what());
    }
}

/**
 * @brief Publishes a canonical entity event with its tenancy in the headers.
 *
 * The payload is the event alone. The subject is the one the event's traits
 * state, and the tenant and the party travel as @c X-Tenant-Id and
 * @c X-Party-Id, so a subscriber that serves people can pass the event on only
 * to a reader who may hear it.
 *
 * @param nats The client to publish with.
 * @param subject The event subject, from @ref event_subject.
 * @param published The event and the tenancy the store reported for it.
 */
template <typename Event>
void publish_entity_event(ores::nats::service::client& nats,
                          const std::string& subject,
                          const domain::published_entity_event<Event>& published) {
    try {
        nats.publish(subject,
                     ores::nats::default_wire_codec().encode(published.event),
                     entity_event_headers(published.tenant_id, published.party_id));
    } catch (const std::exception& e) {
        throw std::runtime_error("Failed to publish to NATS subject '" + subject +
                                 "': " + e.what());
    }
}

}

#endif
