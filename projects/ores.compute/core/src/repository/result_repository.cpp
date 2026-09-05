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
#include "ores.compute.core/repository/result_repository.hpp"
#include "ores.compute.api/domain/result_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.core/repository/result_entity.hpp"
#include "ores.compute.core/repository/result_mapper.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::compute::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string result_repository::sql() {
    return generate_create_table_sql<result_entity>(lg());
}

void result_repository::write(context ctx, const domain::result& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing compute result. " << "id: " << v.id;
    execute_write_query(ctx, result_mapper::map(v), lg(), "Writing compute result to database.");
}

void result_repository::write(context ctx, const std::vector<domain::result>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing compute results. Count: " << v.size();
    execute_write_query(ctx, result_mapper::map(v), lg(), "Writing compute results to database.");
}

std::vector<domain::result> result_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<result_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<result_entity, domain::result>(
        ctx,
        query,
        [](const auto& entities) { return result_mapper::map(entities); },
        lg(),
        "Reading latest compute results");
}

std::vector<domain::result> result_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest compute result. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<result_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<result_entity, domain::result>(
        ctx,
        query,
        [](const auto& entities) { return result_mapper::map(entities); },
        lg(),
        "Reading latest compute result by id.");
}


std::vector<domain::result> result_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all compute result versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<result_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<result_entity, domain::result>(
        ctx,
        query,
        [](const auto& entities) { return result_mapper::map(entities); },
        lg(),
        "Reading all compute result versions by id.");
}

std::optional<domain::result>
result_repository::read_at_version(context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading compute result at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<result_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<result_entity, domain::result>(
        ctx,
        query,
        [](const auto& entities) { return result_mapper::map(entities); },
        lg(),
        "Reading compute result at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::result> result_repository::read_latest_by_workunit_id(
    context ctx, const std::string& workunit_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest compute results. workunit_id: " << workunit_id
                               << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<result_entity>> |
                       where("tenant_id"_c == tid && "workunit_id"_c == workunit_id &&
                             "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<result_entity, domain::result>(
        ctx,
        query,
        [](const auto& entities) { return result_mapper::map(entities); },
        lg(),
        "Reading latest compute results by workunit_id.");
}

std::uint32_t
result_repository::get_total_result_count_by_workunit_id(context ctx,
                                                         const std::string& workunit_id) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active compute results count. workunit_id: "
                               << workunit_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<result_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "workunit_id"_c == workunit_id &&
                             "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active compute results count by workunit_id: " << count;
    return count;
}


void result_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing compute result. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<result_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing compute result from database.");
}

std::vector<domain::result>
result_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest compute results with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<result_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<result_entity, domain::result>(
        ctx,
        query,
        [](const auto& entities) { return result_mapper::map(entities); },
        lg(),
        "Reading latest compute results with pagination.");
}

std::uint32_t result_repository::get_total_result_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active compute result count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<result_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active compute result count: " << count;
    return count;
}

void result_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<result_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing compute results.");
}


std::vector<domain::result> result_repository::read_by_state(context ctx, int server_state) {
    BOOST_LOG_SEV(lg(), debug) << "Reading results by state: " << server_state;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<result_entity>> |
                       where("tenant_id"_c == tid && "server_state"_c == server_state &&
                             "valid_to"_c == max.value());

    return execute_read_query<result_entity, domain::result>(
        ctx,
        query,
        [](const auto& entities) { return result_mapper::map(entities); },
        lg(),
        "Reading results by state.");
}

}
