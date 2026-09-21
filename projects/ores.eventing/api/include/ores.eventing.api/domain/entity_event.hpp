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
#ifndef ORES_EVENTING_API_DOMAIN_ENTITY_EVENT_HPP
#define ORES_EVENTING_API_DOMAIN_ENTITY_EVENT_HPP

#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>

namespace ores::eventing::domain {

/**
 * @brief What happened to the entity an event announces.
 *
 * An event reports what happened and no caller asked for it, so the last
 * segment of an event subject is an action rather than a verb.
 */
enum class event_action { created, updated, deleted };

/**
 * @brief The action's own word, which is also the last subject segment.
 */
[[nodiscard]] constexpr std::string_view to_string(event_action action) {
    switch (action) {
    case event_action::created:
        return "created";
    case event_action::updated:
        return "updated";
    case event_action::deleted:
        return "deleted";
    }
    return "";
}

/**
 * @brief What the store said about a change, before it is typed.
 *
 * The notification trigger publishes this on a Postgres channel as the change
 * happens. The key travels as its own JSON object because it is the entity's
 * key record and only that entity's event type knows its columns; the
 * per-entity conversion turns it into the record.
 *
 * The event a subscriber receives on NATS carries the typed key and no
 * @c entity or @c tenant_id: the subject already names the collection, and
 * tenancy is an authorisation concern rather than part of the announcement.
 */
struct entity_event_notification final {
    /**
     * @brief Identifies this publication.
     *
     * Events are delivered at least once and may arrive more than once, so a
     * consumer recognises a redelivery by this and applying it twice changes
     * nothing.
     */
    std::string event_id;

    /**
     * @brief The entity that changed, as its model spells it.
     *
     * This is what the source maps a notification to a typed event by, so the
     * value is the mapping's key rather than part of the published event.
     */
    std::string entity;

    /**
     * @brief The key of the row that changed, as a JSON object.
     */
    std::string key;

    /**
     * @brief One of created, updated or deleted.
     */
    std::string action;

    /**
     * @brief The version the change produced.
     */
    std::uint32_t version = 0;

    /**
     * @brief When the change was made, in UTC.
     */
    std::chrono::system_clock::time_point occurred_at;

    /**
     * @brief The request that caused the change, when one is known.
     */
    std::optional<std::string> correlation_id;

    /**
     * @brief The tenant that owns the row.
     *
     * The notification carries it so the listener can log and so a subscriber
     * can decide whether the event concerns it; it is not published.
     */
    std::string tenant_id;
};

}

#endif
