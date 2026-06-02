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
#include "ores.controller.core/repository/service_definition_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.controller.core/repository/service_definition_entity.hpp"
#include "ores.controller.core/repository/service_definition_mapper.hpp"

namespace ores::controller::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::vector<api::domain::service_definition>
service_definition_repository::read_latest(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest service definitions.";
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<service_definition_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("service_name"_c);

    return execute_read_query<service_definition_entity,
        api::domain::service_definition>(
        ctx, query,
        [](const auto& entities) {
            return service_definition_mapper::map(entities);
        },
        lg(), "Reading latest service definitions.");
}

void service_definition_repository::save(context ctx,
    const api::domain::service_definition& v) {
    BOOST_LOG_SEV(lg(), debug) << "Saving service definition: " << v.service_name;
    execute_write_query(ctx, service_definition_mapper::map(v), lg(),
        "Saving service definition.");
}

}
