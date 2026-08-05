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
#include "ores.dq.core/repository/lei_relationship_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.dq.api/domain/lei_relationship_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/lei_relationship_entity.hpp"
#include "ores.dq.core/repository/lei_relationship_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string lei_relationship_repository::sql() {
    return generate_create_table_sql<lei_relationship_entity>(lg());
}

void lei_relationship_repository::write(context ctx, const domain::lei_relationship& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing LEI relationship. "
                               << "relationship_start_node_node_id: "
                               << v.relationship_start_node_node_id;
    execute_write_query(
        ctx, lei_relationship_mapper::map(v), lg(), "Writing LEI relationship to database.");
}

void lei_relationship_repository::write(context ctx,
                                        const std::vector<domain::lei_relationship>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing LEI relationships. Count: " << v.size();
    execute_write_query(
        ctx, lei_relationship_mapper::map(v), lg(), "Writing LEI relationships to database.");
}

std::vector<domain::lei_relationship> lei_relationship_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<lei_relationship_entity>> |
                       where("valid_to"_c == max.value()) |
                       order_by("relationship_start_node_node_id"_c);

    return execute_read_query<lei_relationship_entity, domain::lei_relationship>(
        ctx,
        query,
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading latest LEI relationships");
}

std::vector<domain::lei_relationship>
lei_relationship_repository::read_latest(context ctx,
                                         const std::string& relationship_start_node_node_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest LEI relationship. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query =
        sqlgen::read<std::vector<lei_relationship_entity>> |
        where("relationship_start_node_node_id"_c == relationship_start_node_node_id &&
              "valid_to"_c == max.value());

    return execute_read_query<lei_relationship_entity, domain::lei_relationship>(
        ctx,
        query,
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading latest LEI relationship by relationship_start_node_node_id.");
}


std::vector<domain::lei_relationship>
lei_relationship_repository::read_all(context ctx,
                                      const std::string& relationship_start_node_node_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all LEI relationship versions. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id;
    const auto query =
        sqlgen::read<std::vector<lei_relationship_entity>> |
        where("relationship_start_node_node_id"_c == relationship_start_node_node_id) |
        order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<lei_relationship_entity, domain::lei_relationship>(
        ctx,
        query,
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading all LEI relationship versions by relationship_start_node_node_id.");
}

std::optional<domain::lei_relationship> lei_relationship_repository::read_at_version(
    context ctx, const std::string& relationship_start_node_node_id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading LEI relationship at version. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id << " version: " << version;
    const auto query =
        sqlgen::read<std::vector<lei_relationship_entity>> |
        where("relationship_start_node_node_id"_c == relationship_start_node_node_id &&
              "version"_c == version) |
        sqlgen::limit(1);

    const auto entities = execute_read_query<lei_relationship_entity, domain::lei_relationship>(
        ctx,
        query,
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading LEI relationship at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void lei_relationship_repository::remove(context ctx,
                                         const std::string& relationship_start_node_node_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing LEI relationship. "
                               << "relationship_start_node_node_id: "
                               << relationship_start_node_node_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<lei_relationship_entity> |
        where("tenant_id"_c == tid &&
              "relationship_start_node_node_id"_c == relationship_start_node_node_id &&
              "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing LEI relationship from database.");
}

std::vector<domain::lei_relationship>
lei_relationship_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest LEI relationships with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<lei_relationship_entity>> |
                       where("valid_to"_c == max.value()) |
                       order_by("relationship_start_node_node_id"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<lei_relationship_entity, domain::lei_relationship>(
        ctx,
        query,
        [](const auto& entities) { return lei_relationship_mapper::map(entities); },
        lg(),
        "Reading latest LEI relationships with pagination.");
}

std::uint32_t lei_relationship_repository::get_total_relationship_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active LEI relationship count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<lei_relationship_entity>(sqlgen::count().as<"count">()) |
                       where("valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active LEI relationship count: " << count;
    return count;
}

void lei_relationship_repository::remove(
    context ctx, const std::vector<std::string>& relationship_start_node_node_ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<lei_relationship_entity> |
        where("tenant_id"_c == tid &&
              "relationship_start_node_node_id"_c.in(relationship_start_node_node_ids) &&
              "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing LEI relationships.");
}


}
