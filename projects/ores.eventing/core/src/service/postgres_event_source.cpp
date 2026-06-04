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
#include <rfl/json.hpp>

namespace ores::eventing::service {

using namespace ores::logging;

postgres_event_source::postgres_event_source(database::context ctx, event_bus& bus)
    : bus_(bus)
    , listener_(std::move(ctx), [this](const std::string& /*channel*/, const std::string& payload) {
        try {
            auto result = rfl::json::read<domain::entity_change_event>(payload);
            if (result) {
                on_entity_change(*result);
            } else {
                const auto n = ++parse_failure_count_;
                BOOST_LOG_SEV(lg(), error) << "Failed to deserialize notification payload"
                                           << " (total failures: " << n << "): " << payload;
            }
        } catch (const std::exception& e) {
            const auto n = ++parse_failure_count_;
            BOOST_LOG_SEV(lg(), error)
                << "Exception parsing notification payload"
                << " (total failures: " << n << "): " << payload << " — " << e.what();
        }
    }) {
    BOOST_LOG_SEV(lg(), debug) << "Postgres event source created.";
}

postgres_event_source::~postgres_event_source() {
    stop();
    BOOST_LOG_SEV(lg(), debug) << "Postgres event source destroyed.";
}

void postgres_event_source::start() {
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
