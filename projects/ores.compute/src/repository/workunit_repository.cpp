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
#include "ores.compute/repository/workunit_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.compute/domain/workunit_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute/repository/workunit_entity.hpp"
#include "ores.compute/repository/workunit_mapper.hpp"

namespace ores::compute::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string workunit_repository::sql() {
    return generate_create_table_sql<workunit_entity>(lg());
}

void workunit_repository::write(context ctx, const domain::workunit& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing workunit: " << v.id;
    execute_write_query(ctx, workunit_mapper::map(v),
        lg(), "Writing workunit to database.");
}

void workunit_repository::write(
    context ctx, const std::vector<domain::workunit>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing workunits. Count: " << v.size();
    execute_write_query(ctx, workunit_mapper::map(v),
        lg(), "Writing workunits to database.");
}

std::vector<domain::workunit>
workunit_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<workunit_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<workunit_entity, domain::workunit>(
        ctx, query,
        [](const auto& entities) { return workunit_mapper::map(entities); },
        lg(), "Reading latest workunits");
}

std::vector<domain::workunit>
workunit_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest workunit. id: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<workunit_entity>> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<workunit_entity, domain::workunit>(
        ctx, query,
        [](const auto& entities) { return workunit_mapper::map(entities); },
        lg(), "Reading latest workunit by id.");
}

std::vector<domain::workunit>
workunit_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all workunit versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<workunit_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<workunit_entity, domain::workunit>(
        ctx, query,
        [](const auto& entities) { return workunit_mapper::map(entities); },
        lg(), "Reading all workunit versions by id.");
}

std::vector<domain::workunit>
workunit_repository::read_by_batch(context ctx, const std::string& batch_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading workunits by batch: " << batch_id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<workunit_entity>> |
        where("tenant_id"_c == tid && "batch_id"_c == batch_id
            && "valid_to"_c == max.value());

    return execute_read_query<workunit_entity, domain::workunit>(
        ctx, query,
        [](const auto& entities) { return workunit_mapper::map(entities); },
        lg(), "Reading workunits by batch.");
}

void workunit_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing workunit: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<workunit_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing workunit from database.");
}

}
