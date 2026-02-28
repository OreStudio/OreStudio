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
#include "ores.iam/repository/tenant_type_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam/domain/tenant_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/repository/tenant_type_entity.hpp"
#include "ores.iam/repository/tenant_type_mapper.hpp"

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string tenant_type_repository::sql() {
    return generate_create_table_sql<tenant_type_entity>(lg());
}

void tenant_type_repository::
write(context ctx, const domain::tenant_type& type) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenant type to database: "
                               << type.type;
    auto entity = tenant_type_mapper::map(type);
    entity.tenant_id = ctx.tenant_id().to_string();
    execute_write_query(ctx, entity,
        lg(), "Writing tenant type to database.");
}

void tenant_type_repository::
write(context ctx, const std::vector<domain::tenant_type>& types) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenant types to database. Count: "
                               << types.size();
    auto entities = tenant_type_mapper::map(types);
    for (auto& entity : entities) {
        entity.tenant_id = ctx.tenant_id().to_string();
    }
    execute_write_query(ctx, entities,
        lg(), "Writing tenant types to database.");
}

std::vector<domain::tenant_type>
tenant_type_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tenant_type_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<tenant_type_entity, domain::tenant_type>(
        ctx, query,
        [](const auto& entities) { return tenant_type_mapper::map(entities); },
        lg(), "Reading latest tenant types");
}

std::vector<domain::tenant_type>
tenant_type_repository::read_latest(context ctx, const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenant type. Type: " << type;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tenant_type_entity>> |
        where("type"_c == type && "valid_to"_c == max.value());

    return execute_read_query<tenant_type_entity, domain::tenant_type>(
        ctx, query,
        [](const auto& entities) { return tenant_type_mapper::map(entities); },
        lg(), "Reading latest tenant type by type.");
}

std::vector<domain::tenant_type>
tenant_type_repository::read_all(context ctx, const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all tenant type versions. Type: "
                               << type;

    const auto query = sqlgen::read<std::vector<tenant_type_entity>> |
        where("type"_c == type) |
        order_by("version"_c.desc());

    return execute_read_query<tenant_type_entity, domain::tenant_type>(
        ctx, query,
        [](const auto& entities) { return tenant_type_mapper::map(entities); },
        lg(), "Reading all tenant type versions by type.");
}

void tenant_type_repository::remove(context ctx, const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenant type from database: "
                               << type;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::delete_from<tenant_type_entity> |
        where("type"_c == type && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(),
        "Removing tenant type from database.");
}

void tenant_type_repository::
remove(context ctx, const std::vector<std::string>& types) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::delete_from<tenant_type_entity> |
        where("type"_c.in(types) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "batch removing tenant_types");
}

}
