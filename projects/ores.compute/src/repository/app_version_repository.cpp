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
#include "ores.compute/repository/app_version_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.compute/domain/app_version_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute/repository/app_version_entity.hpp"
#include "ores.compute/repository/app_version_mapper.hpp"

namespace ores::compute::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string app_version_repository::sql() {
    return generate_create_table_sql<app_version_entity>(lg());
}

void app_version_repository::write(context ctx, const domain::app_version& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing app version: " << v.id;
    execute_write_query(ctx, app_version_mapper::map(v),
        lg(), "Writing app version to database.");
}

void app_version_repository::write(
    context ctx, const std::vector<domain::app_version>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing app versions. Count: " << v.size();
    execute_write_query(ctx, app_version_mapper::map(v),
        lg(), "Writing app versions to database.");
}

std::vector<domain::app_version>
app_version_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<app_version_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<app_version_entity, domain::app_version>(
        ctx, query,
        [](const auto& entities) { return app_version_mapper::map(entities); },
        lg(), "Reading latest app versions");
}

std::vector<domain::app_version>
app_version_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest app version. id: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<app_version_entity>> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<app_version_entity, domain::app_version>(
        ctx, query,
        [](const auto& entities) { return app_version_mapper::map(entities); },
        lg(), "Reading latest app version by id.");
}

std::vector<domain::app_version>
app_version_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all app version versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<app_version_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<app_version_entity, domain::app_version>(
        ctx, query,
        [](const auto& entities) { return app_version_mapper::map(entities); },
        lg(), "Reading all app version versions by id.");
}

void app_version_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing app version: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<app_version_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing app version from database.");
}

}
