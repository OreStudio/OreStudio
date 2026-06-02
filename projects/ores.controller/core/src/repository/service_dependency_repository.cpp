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
#include "ores.controller.core/repository/service_dependency_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.controller.core/repository/service_dependency_entity.hpp"

namespace ores::controller::repository {

using namespace sqlgen;
using namespace ores::logging;
using namespace ores::database::repository;

std::vector<std::pair<std::string, std::string>>
service_dependency_repository::read_all(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading service dependencies.";

    const auto query = sqlgen::read<std::vector<service_dependency_entity>>;
    const auto entities = execute_read_query<service_dependency_entity,
        service_dependency_entity>(
        ctx, query,
        [](const auto& v) { return v; },
        lg(), "Reading service dependencies.");

    std::vector<std::pair<std::string, std::string>> result;
    result.reserve(entities.size());
    for (const auto& e : entities)
        result.emplace_back(e.service_name.value(), e.depends_on.value());
    return result;
}

}
