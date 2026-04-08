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
#ifndef ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_EVENT_ENTITY_HPP
#define ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_EVENT_ENTITY_HPP

#include <string>
#include <optional>
#include <ostream>
#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::controller::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Database entity for a service lifecycle event (Kubernetes Event analogue).
 *
 * Maps to ores_controller_service_events_tbl, a TimescaleDB hypertable
 * partitioned by occurred_at. Insert-only: no UPDATE operations.
 */
struct service_event_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename =
        "ores_controller_service_events_tbl";

    sqlgen::PrimaryKey<db_timestamp> occurred_at;
    sqlgen::PrimaryKey<std::string> event_id;
    std::string service_name;
    std::optional<std::string> instance_id;
    std::optional<int> replica_index;
    std::string event_type;
    std::string message;
};

std::ostream& operator<<(std::ostream& s, const service_event_entity& v);

}

#endif
