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
#include "ores.dq.core/repository/lei_entity_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.dq.api/domain/lei_entity_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/lei_entity_entity.hpp"
#include "ores.dq.core/repository/lei_entity_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string lei_entity_repository::sql() {
    return generate_create_table_sql<lei_entity_entity>(lg());
}

void lei_entity_repository::write(context ctx, const domain::lei_entity& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing LEI entity. " << "lei: " << v.lei;
    execute_write_query(ctx, lei_entity_mapper::map(v), lg(), "Writing LEI entity to database.");
}

void lei_entity_repository::write(context ctx, const std::vector<domain::lei_entity>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing LEI entities. Count: " << v.size();
    execute_write_query(ctx, lei_entity_mapper::map(v), lg(), "Writing LEI entities to database.");
}

std::vector<domain::lei_entity> lei_entity_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<lei_entity_entity>> |
                       where("valid_to"_c == max.value()) | order_by("lei"_c);

    return execute_read_query<lei_entity_entity, domain::lei_entity>(
        ctx,
        query,
        [](const auto& entities) { return lei_entity_mapper::map(entities); },
        lg(),
        "Reading latest LEI entities");
}

std::vector<domain::lei_entity> lei_entity_repository::read_latest(context ctx,
                                                                   const std::string& lei) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest LEI entity. " << "lei: " << lei;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<lei_entity_entity>> |
                       where("lei"_c == lei && "valid_to"_c == max.value());

    return execute_read_query<lei_entity_entity, domain::lei_entity>(
        ctx,
        query,
        [](const auto& entities) { return lei_entity_mapper::map(entities); },
        lg(),
        "Reading latest LEI entity by lei.");
}


std::vector<domain::lei_entity> lei_entity_repository::read_all(context ctx,
                                                                const std::string& lei) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all LEI entity versions. " << "lei: " << lei;
    const auto query = sqlgen::read<std::vector<lei_entity_entity>> | where("lei"_c == lei) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<lei_entity_entity, domain::lei_entity>(
        ctx,
        query,
        [](const auto& entities) { return lei_entity_mapper::map(entities); },
        lg(),
        "Reading all LEI entity versions by lei.");
}

std::optional<domain::lei_entity>
lei_entity_repository::read_at_version(context ctx, const std::string& lei, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading LEI entity at version. " << "lei: " << lei
                               << " version: " << version;
    const auto query = sqlgen::read<std::vector<lei_entity_entity>> |
                       where("lei"_c == lei && "version"_c == version) | sqlgen::limit(1);

    const auto entities = execute_read_query<lei_entity_entity, domain::lei_entity>(
        ctx,
        query,
        [](const auto& entities) { return lei_entity_mapper::map(entities); },
        lg(),
        "Reading LEI entity at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void lei_entity_repository::remove(context ctx, const std::string& lei) {
    BOOST_LOG_SEV(lg(), debug) << "Removing LEI entity. " << "lei: " << lei;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<lei_entity_entity> |
                       where("tenant_id"_c == tid && "lei"_c == lei && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing LEI entity from database.");
}

std::vector<domain::lei_entity>
lei_entity_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest LEI entities with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<lei_entity_entity>> |
                       where("valid_to"_c == max.value()) | order_by("lei"_c) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<lei_entity_entity, domain::lei_entity>(
        ctx,
        query,
        [](const auto& entities) { return lei_entity_mapper::map(entities); },
        lg(),
        "Reading latest LEI entities with pagination.");
}

std::uint32_t lei_entity_repository::get_total_entity_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active LEI entity count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<lei_entity_entity>(sqlgen::count().as<"count">()) |
                       where("valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active LEI entity count: " << count;
    return count;
}

void lei_entity_repository::remove(context ctx, const std::vector<std::string>& leis) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<lei_entity_entity> |
        where("tenant_id"_c == tid && "lei"_c.in(leis) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing LEI entities.");
}


}
