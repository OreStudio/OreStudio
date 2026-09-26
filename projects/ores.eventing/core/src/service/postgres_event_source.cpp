/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.eventing.core/service/postgres_event_source.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <map>
#include <rfl/json.hpp>

namespace ores::eventing::service {

using namespace ores::logging;

postgres_event_source::postgres_event_source(database::context ctx, event_bus& bus)
    : bus_(bus)
    , listener_(std::move(ctx), [this](const std::string& channel, const std::string& payload) {
        // Every channel carries the same payload: the trigger's
        // canonical notification. A channel with a canonical
        // mapping turns it into the typed event its traits name. A
        // channel with an older mapping hands the change to an
        // in-process subscriber that needs the tenant and the
        // changed ids, so the notification is converted rather than
        // parsed as an entity_change_event -- parsing it as one
        // fails on every notification, because the trigger stopped
        // emitting that shape. One channel may have both.
        try {
            const auto notification = rfl::json::read<domain::entity_event_notification>(payload);
            if (!notification) {
                const auto n = ++parse_failure_count_;
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize notification payload"
                                           << " (total failures: " << n << "): " << payload;
                return;
            }
            const auto canonical = entity_event_mappings_.find(channel);
            if (canonical != entity_event_mappings_.end())
                canonical->second.publisher(*notification);

            const auto legacy = channel_entities_.find(channel);
            if (legacy != channel_entities_.end()) {
                const auto mapping = entity_mappings_.find(legacy->second);
                if (mapping != entity_mappings_.end()) {
                    domain::entity_change_event change;
                    change.entity = notification->entity;
                    change.timestamp = notification->occurred_at;
                    change.tenant_id = notification->tenant_id;
                    if (const auto keys =
                            rfl::json::read<std::map<std::string, std::string>>(notification->key))
                        for (const auto& entry : *keys)
                            change.entity_ids.push_back(entry.second);
                    mapping->second.publisher(
                        change.timestamp, change.entity_ids, change.tenant_id);
                }
            }

            if (canonical == entity_event_mappings_.end() && legacy == channel_entities_.end())
                BOOST_LOG_SEV(lg(), warn) << "No mapping registered for channel: '" << channel
                                          << "' - notification ignored";
        } catch (const std::exception& e) {
            const auto n = ++parse_failure_count_;
            BOOST_LOG_SEV(lg(), error)
                << "Exception parsing notification payload"
                << " (total failures: " << n << "): " << payload << " - " << e.what();
        }
    }) {
    BOOST_LOG_SEV(lg(), debug) << "Postgres event source created.";
}

postgres_event_source::~postgres_event_source() {
    stop();
    BOOST_LOG_SEV(lg(), debug) << "Postgres event source destroyed.";
}

void postgres_event_source::start() {
    registered_entities_.clear();
    for (const auto& kv : entity_mappings_) {
        if (!registered_entities_.empty())
            registered_entities_ += ", ";
        registered_entities_ += kv.first;
    }
    BOOST_LOG_SEV(lg(), info) << "Starting postgres event source. Registered entities: ["
                              << registered_entities_ << "]";
    listener_.start();
}

void postgres_event_source::stop() {
    BOOST_LOG_SEV(lg(), info) << "Stopping postgres event source.";
    listener_.stop();
}

bool postgres_event_source::wait_until_ready(std::chrono::milliseconds timeout) {
    return listener_.wait_until_ready(timeout);
}

void postgres_event_source::on_entity_event(const std::string& channel,
                                            const domain::entity_event_notification& e) {
    BOOST_LOG_SEV(lg(), info) << "Received event notification for entity: " << e.entity
                              << " action: " << e.action << " version: " << e.version;

    const auto it = entity_event_mappings_.find(channel);
    if (it == entity_event_mappings_.end()) {
        BOOST_LOG_SEV(lg(), warn) << "No event mapping registered for channel: '" << channel
                                  << "' - notification ignored";
        return;
    }

    try {
        it->second.publisher(e);
        BOOST_LOG_SEV(lg(), debug) << "Successfully published event for entity: " << e.entity;
    } catch (const std::exception& ex) {
        BOOST_LOG_SEV(lg(), error)
            << "Exception while publishing event for entity '" << e.entity << "': " << ex.what();
    }
}

void postgres_event_source::on_entity_change(const domain::entity_change_event& e) {
    BOOST_LOG_SEV(lg(), info) << "Received PostgreSQL notification for entity: " << e.entity
                              << " with " << e.entity_ids.size() << " entity IDs";

    auto it = entity_mappings_.find(e.entity);
    if (it == entity_mappings_.end()) {
        BOOST_LOG_SEV(lg(), warn) << "No event mapping registered for entity: '" << e.entity
                                  << "'. Registered: [" << registered_entities_
                                  << "] - notification ignored";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Dispatching to registered publisher for entity: " << e.entity;

    try {
        it->second.publisher(e.timestamp, e.entity_ids, e.tenant_id);
        BOOST_LOG_SEV(lg(), debug) << "Successfully published event for entity: " << e.entity;
    } catch (const std::exception& ex) {
        BOOST_LOG_SEV(lg(), error)
            << "Exception while publishing event for entity '" << e.entity << "': " << ex.what();
    }
}

}
