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
#include "ores.iam/repository/tenant_status_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam/domain/tenant_status_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/repository/tenant_status_entity.hpp"
#include "ores.iam/repository/tenant_status_mapper.hpp"

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string tenant_status_repository::sql() {
    return generate_create_table_sql<tenant_status_entity>(lg());
}

void tenant_status_repository::
write(context ctx, const domain::tenant_status& status) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenant status to database: "
                               << status.status;
    auto entity = tenant_status_mapper::map(status);
    entity.tenant_id = ctx.tenant_id().to_string();
    execute_write_query(ctx, entity,
        lg(), "Writing tenant status to database.");
}

void tenant_status_repository::
write(context ctx, const std::vector<domain::tenant_status>& statuses) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenant statuses to database. Count: "
                               << statuses.size();
    auto entities = tenant_status_mapper::map(statuses);
    for (auto& entity : entities) {
        entity.tenant_id = ctx.tenant_id().to_string();
    }
    execute_write_query(ctx, entities,
        lg(), "Writing tenant statuses to database.");
}

std::vector<domain::tenant_status>
tenant_status_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tenant_status_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<tenant_status_entity, domain::tenant_status>(
        ctx, query,
        [](const auto& entities) { return tenant_status_mapper::map(entities); },
        lg(), "Reading latest tenant statuses");
}

std::vector<domain::tenant_status>
tenant_status_repository::read_latest(context ctx, const std::string& status) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenant status. Status: "
                               << status;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tenant_status_entity>> |
        where("status"_c == status && "valid_to"_c == max.value());

    return execute_read_query<tenant_status_entity, domain::tenant_status>(
        ctx, query,
        [](const auto& entities) { return tenant_status_mapper::map(entities); },
        lg(), "Reading latest tenant status by status.");
}

std::vector<domain::tenant_status>
tenant_status_repository::read_all(context ctx, const std::string& status) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all tenant status versions. Status: "
                               << status;

    const auto query = sqlgen::read<std::vector<tenant_status_entity>> |
        where("status"_c == status) |
        order_by("version"_c.desc());

    return execute_read_query<tenant_status_entity, domain::tenant_status>(
        ctx, query,
        [](const auto& entities) { return tenant_status_mapper::map(entities); },
        lg(), "Reading all tenant status versions by status.");
}

void tenant_status_repository::remove(context ctx, const std::string& status) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenant status from database: "
                               << status;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::delete_from<tenant_status_entity> |
        where("status"_c == status && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(),
        "Removing tenant status from database.");
}

}
