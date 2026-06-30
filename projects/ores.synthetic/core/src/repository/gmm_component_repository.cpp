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
#include "ores.synthetic.core/repository/gmm_component_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.synthetic.api/domain/gmm_component_json_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.core/repository/gmm_component_entity.hpp"
#include "ores.synthetic.core/repository/gmm_component_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::synthetic::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string gmm_component_repository::sql() {
    return generate_create_table_sql<gmm_component_entity>(lg());
}

void gmm_component_repository::write(context ctx, const domain::gmm_component& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing GMM component: " << v.id;
    execute_write_query(
        ctx, gmm_component_mapper::map(v), lg(), "Writing GMM component to database.");
}

void gmm_component_repository::write(context ctx, const std::vector<domain::gmm_component>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing GMM components. Count: " << v.size();
    execute_write_query(
        ctx, gmm_component_mapper::map(v), lg(), "Writing GMM components to database.");
}

std::vector<domain::gmm_component> gmm_component_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<gmm_component_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<gmm_component_entity, domain::gmm_component>(
        ctx,
        query,
        [](const auto& entities) { return gmm_component_mapper::map(entities); },
        lg(),
        "Reading latest GMM components");
}

std::vector<domain::gmm_component> gmm_component_repository::read_latest(context ctx,
                                                                         const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest GMM component. id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<gmm_component_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<gmm_component_entity, domain::gmm_component>(
        ctx,
        query,
        [](const auto& entities) { return gmm_component_mapper::map(entities); },
        lg(),
        "Reading latest GMM component by id.");
}

std::vector<domain::gmm_component> gmm_component_repository::read_all(context ctx,
                                                                      const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all GMM component versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<gmm_component_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) | order_by("version"_c.desc());

    return execute_read_query<gmm_component_entity, domain::gmm_component>(
        ctx,
        query,
        [](const auto& entities) { return gmm_component_mapper::map(entities); },
        lg(),
        "Reading all GMM component versions by id.");
}

void gmm_component_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing GMM component: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<gmm_component_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing GMM component from database.");
}

std::vector<domain::gmm_component>
gmm_component_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest GMM components with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<gmm_component_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<gmm_component_entity, domain::gmm_component>(
        ctx,
        query,
        [](const auto& entities) { return gmm_component_mapper::map(entities); },
        lg(),
        "Reading latest GMM components with pagination.");
}

std::uint32_t gmm_component_repository::get_total_gmm_component_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active GMM component count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<gmm_component_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active GMM component count: " << count;
    return count;
}

void gmm_component_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<gmm_component_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing GMM components.");
}


}
