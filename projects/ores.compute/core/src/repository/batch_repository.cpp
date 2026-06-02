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
#include "ores.compute.core/repository/batch_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.compute.api/domain/batch_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.core/repository/batch_entity.hpp"
#include "ores.compute.core/repository/batch_mapper.hpp"

namespace ores::compute::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string batch_repository::sql() {
    return generate_create_table_sql<batch_entity>(lg());
}

void batch_repository::write(context ctx, const domain::batch& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing compute batch: " << v.id;
    execute_write_query(ctx, batch_mapper::map(v),
        lg(), "Writing compute batch to database.");
}

void batch_repository::write(
    context ctx, const std::vector<domain::batch>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing compute batches. Count: " << v.size();
    execute_write_query(ctx, batch_mapper::map(v),
        lg(), "Writing compute batches to database.");
}

std::vector<domain::batch>
batch_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<batch_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<batch_entity, domain::batch>(
        ctx, query,
        [](const auto& entities) { return batch_mapper::map(entities); },
        lg(), "Reading latest compute batches");
}

std::vector<domain::batch>
batch_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest compute batch. id: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<batch_entity>> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<batch_entity, domain::batch>(
        ctx, query,
        [](const auto& entities) { return batch_mapper::map(entities); },
        lg(), "Reading latest compute batch by id.");
}

std::vector<domain::batch>
batch_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all compute batch versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<batch_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<batch_entity, domain::batch>(
        ctx, query,
        [](const auto& entities) { return batch_mapper::map(entities); },
        lg(), "Reading all compute batch versions by id.");
}

void batch_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing compute batch: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<batch_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing compute batch from database.");
}

}
