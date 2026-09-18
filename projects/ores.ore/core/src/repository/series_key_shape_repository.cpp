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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_repository.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.ore.core/repository/series_key_shape_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.ore.api/domain/series_key_shape_json_io.hpp" // IWYU pragma: keep.
#include "ores.ore.core/repository/series_key_shape_entity.hpp"
#include "ores.ore.core/repository/series_key_shape_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::ore::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string series_key_shape_repository::sql() {
    return generate_create_table_sql<series_key_shape_entity>(lg());
}

void series_key_shape_repository::write(context ctx, const domain::series_key_shape& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing series key shape. " << "series_type: " << v.series_type;
    execute_write_query(
        ctx, series_key_shape_mapper::map(v), lg(), "Writing series key shape to database.");
}

void series_key_shape_repository::write(context ctx,
                                        const std::vector<domain::series_key_shape>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing series key shapes. Count: " << v.size();
    execute_write_query(
        ctx, series_key_shape_mapper::map(v), lg(), "Writing series key shapes to database.");
}

std::vector<domain::series_key_shape> series_key_shape_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<series_key_shape_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("series_type"_c);

    return execute_read_query<series_key_shape_entity, domain::series_key_shape>(
        ctx,
        query,
        [](const auto& entities) { return series_key_shape_mapper::map(entities); },
        lg(),
        "Reading latest series key shapes");
}

std::vector<domain::series_key_shape>
series_key_shape_repository::read_latest(context ctx, const std::string& series_type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest series key shape. "
                               << "series_type: " << series_type;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<series_key_shape_entity>> |
                       where("tenant_id"_c == tid && "series_type"_c == series_type &&
                             "valid_to"_c == max.value());

    return execute_read_query<series_key_shape_entity, domain::series_key_shape>(
        ctx,
        query,
        [](const auto& entities) { return series_key_shape_mapper::map(entities); },
        lg(),
        "Reading latest series key shape by series_type.");
}


std::vector<domain::series_key_shape>
series_key_shape_repository::read_all(context ctx, const std::string& series_type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all series key shape versions. "
                               << "series_type: " << series_type;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<series_key_shape_entity>> |
                       where("tenant_id"_c == tid && "series_type"_c == series_type) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<series_key_shape_entity, domain::series_key_shape>(
        ctx,
        query,
        [](const auto& entities) { return series_key_shape_mapper::map(entities); },
        lg(),
        "Reading all series key shape versions by series_type.");
}

std::optional<domain::series_key_shape> series_key_shape_repository::read_at_version(
    context ctx, const std::string& series_type, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading series key shape at version. "
                               << "series_type: " << series_type << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<series_key_shape_entity>> |
        where("tenant_id"_c == tid && "series_type"_c == series_type && "version"_c == version) |
        sqlgen::limit(1);

    const auto entities = execute_read_query<series_key_shape_entity, domain::series_key_shape>(
        ctx,
        query,
        [](const auto& entities) { return series_key_shape_mapper::map(entities); },
        lg(),
        "Reading series key shape at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void series_key_shape_repository::remove(context ctx, const std::string& series_type) {
    BOOST_LOG_SEV(lg(), debug) << "Removing series key shape. " << "series_type: " << series_type;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<series_key_shape_entity> |
                       where("tenant_id"_c == tid && "series_type"_c == series_type &&
                             "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing series key shape from database.");
}

std::vector<domain::series_key_shape>
series_key_shape_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest series key shapes with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<series_key_shape_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("series_type"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<series_key_shape_entity, domain::series_key_shape>(
        ctx,
        query,
        [](const auto& entities) { return series_key_shape_mapper::map(entities); },
        lg(),
        "Reading latest series key shapes with pagination.");
}

std::uint32_t series_key_shape_repository::get_total_shape_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active series key shape count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<series_key_shape_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active series key shape count: " << count;
    return count;
}

void series_key_shape_repository::remove(context ctx,
                                         const std::vector<std::string>& series_types) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<series_key_shape_entity> |
                       where("tenant_id"_c == tid && "series_type"_c.in(series_types) &&
                             "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing series key shapes.");
}


}
