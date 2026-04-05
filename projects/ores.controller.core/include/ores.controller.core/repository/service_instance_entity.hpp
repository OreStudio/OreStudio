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
#ifndef ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_INSTANCE_ENTITY_HPP
#define ORES_CONTROLLER_CORE_REPOSITORY_SERVICE_INSTANCE_ENTITY_HPP

#include <string>
#include <optional>
#include <ostream>
#include "sqlgen/Timestamp.hpp"
#include "sqlgen/PrimaryKey.hpp"

namespace ores::controller::repository {

/**
 * @brief Database entity for a running service instance (Kubernetes Pod analogue).
 *
 * Maps to ores_controller_service_instances_tbl. Non-temporal, no tenant_id.
 */
struct service_instance_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename =
        "ores_controller_service_instances_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string service_name;
    int replica_index = 0;
    std::optional<int> pid;
    std::string phase{"pending"};
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> started_at;
    std::optional<sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">> stopped_at;
    int restart_count = 0;
    sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S"> created_at;
};

std::ostream& operator<<(std::ostream& s, const service_instance_entity& v);

}

#endif
