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
#include "ores.controller.core/repository/service_event_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.controller.core/repository/service_event_entity.hpp"
#include "ores.controller.core/repository/service_event_mapper.hpp"

namespace ores::controller::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::vector<api::domain::service_event>
service_event_repository::read_latest(context ctx,
    const std::string& service_name_filter, int limit) {

    BOOST_LOG_SEV(lg(), debug) << "Reading service events. Filter: '"
                               << service_name_filter
                               << "' limit=" << limit;

    if (!service_name_filter.empty()) {
        const auto query =
            sqlgen::read<std::vector<service_event_entity>> |
            where("service_name"_c == service_name_filter) |
            order_by("occurred_at"_c.desc()) |
            sqlgen::limit(limit);
        return execute_read_query<service_event_entity, api::domain::service_event>(
            ctx, query,
            [](const auto& e) { return service_event_mapper::map(e); },
            lg(), "Reading service events by service.");
    }

    const auto query = sqlgen::read<std::vector<service_event_entity>> |
        order_by("occurred_at"_c.desc()) |
        sqlgen::limit(limit);
    return execute_read_query<service_event_entity, api::domain::service_event>(
        ctx, query,
        [](const auto& e) { return service_event_mapper::map(e); },
        lg(), "Reading recent service events.");
}

void service_event_repository::insert(context ctx,
    const api::domain::service_event& ev) {

    BOOST_LOG_SEV(lg(), debug) << "Inserting event: "
                               << ev.service_name << " / " << ev.event_type;
    execute_write_query(ctx, service_event_mapper::map(ev), lg(),
        "Inserting service event.");
}

}
