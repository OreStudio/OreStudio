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
#include "ores.eventing/service/postgres_event_source.hpp"

namespace ores::eventing::service {

using namespace ores::utility::log;

postgres_event_source::postgres_event_source(
    utility::database::context ctx, event_bus& bus)
    : bus_(bus),
      listener_(std::move(ctx),
          [this](const domain::entity_change_event& e) {
              on_entity_change(e);
          }) {
    BOOST_LOG_SEV(lg(), debug) << "Postgres event source created.";
}

postgres_event_source::~postgres_event_source() {
    stop();
    BOOST_LOG_SEV(lg(), debug) << "Postgres event source destroyed.";
}

void postgres_event_source::start() {
    BOOST_LOG_SEV(lg(), info) << "Starting postgres event source with "
                              << entity_mappings_.size() << " registered mappings.";
    listener_.start();
}

void postgres_event_source::stop() {
    BOOST_LOG_SEV(lg(), info) << "Stopping postgres event source.";
    listener_.stop();
}

void postgres_event_source::on_entity_change(const domain::entity_change_event& e) {
    BOOST_LOG_SEV(lg(), info)
        << "Received PostgreSQL notification for entity: " << e.entity;

    auto it = entity_mappings_.find(e.entity);
    if (it == entity_mappings_.end()) {
        BOOST_LOG_SEV(lg(), warn)
            << "No event mapping registered for entity: " << e.entity
            << " - notification will be ignored";
        return;
    }

    BOOST_LOG_SEV(lg(), debug)
        << "Dispatching to registered publisher for entity: " << e.entity;

    try {
        it->second.publisher(e.timestamp);
        BOOST_LOG_SEV(lg(), debug)
            << "Successfully published event for entity: " << e.entity;
    } catch (const std::exception& ex) {
        BOOST_LOG_SEV(lg(), error)
            << "Exception while publishing event for entity '" << e.entity
            << "': " << ex.what();
    }
}

}
