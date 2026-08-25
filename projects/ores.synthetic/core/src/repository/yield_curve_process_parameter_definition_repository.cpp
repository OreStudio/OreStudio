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
#include "ores.synthetic.core/repository/yield_curve_process_parameter_definition_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition_json_io.hpp" // IWYU pragma: keep.
#include "ores.synthetic.core/repository/yield_curve_process_parameter_definition_entity.hpp"
#include "ores.synthetic.core/repository/yield_curve_process_parameter_definition_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::synthetic::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string yield_curve_process_parameter_definition_repository::sql() {
    return generate_create_table_sql<yield_curve_process_parameter_definition_entity>(lg());
}

void yield_curve_process_parameter_definition_repository::write(
    context ctx, const domain::yield_curve_process_parameter_definition& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing yield curve process parameter definition. "
                               << "id: " << v.id;
    execute_write_query(ctx,
                        yield_curve_process_parameter_definition_mapper::map(v),
                        lg(),
                        "Writing yield curve process parameter definition to database.");
}

void yield_curve_process_parameter_definition_repository::write(
    context ctx, const std::vector<domain::yield_curve_process_parameter_definition>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing yield curve process parameter definitions. Count: "
                               << v.size();
    execute_write_query(ctx,
                        yield_curve_process_parameter_definition_mapper::map(v),
                        lg(),
                        "Writing yield curve process parameter definitions to database.");
}

std::vector<domain::yield_curve_process_parameter_definition>
yield_curve_process_parameter_definition_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<yield_curve_process_parameter_definition_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<yield_curve_process_parameter_definition_entity,
                              domain::yield_curve_process_parameter_definition>(
        ctx,
        query,
        [](const auto& entities) {
            return yield_curve_process_parameter_definition_mapper::map(entities);
        },
        lg(),
        "Reading latest yield curve process parameter definitions");
}

std::vector<domain::yield_curve_process_parameter_definition>
yield_curve_process_parameter_definition_repository::read_latest(context ctx,
                                                                 const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest yield curve process parameter definition. "
                               << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<yield_curve_process_parameter_definition_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<yield_curve_process_parameter_definition_entity,
                              domain::yield_curve_process_parameter_definition>(
        ctx,
        query,
        [](const auto& entities) {
            return yield_curve_process_parameter_definition_mapper::map(entities);
        },
        lg(),
        "Reading latest yield curve process parameter definition by id.");
}


std::vector<domain::yield_curve_process_parameter_definition>
yield_curve_process_parameter_definition_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all yield curve process parameter definition versions. "
                               << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<yield_curve_process_parameter_definition_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<yield_curve_process_parameter_definition_entity,
                              domain::yield_curve_process_parameter_definition>(
        ctx,
        query,
        [](const auto& entities) {
            return yield_curve_process_parameter_definition_mapper::map(entities);
        },
        lg(),
        "Reading all yield curve process parameter definition versions by id.");
}

std::optional<domain::yield_curve_process_parameter_definition>
yield_curve_process_parameter_definition_repository::read_at_version(context ctx,
                                                                     const std::string& id,
                                                                     std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading yield curve process parameter definition at version. "
                               << "id: " << id << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<yield_curve_process_parameter_definition_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<yield_curve_process_parameter_definition_entity,
                                             domain::yield_curve_process_parameter_definition>(
        ctx,
        query,
        [](const auto& entities) {
            return yield_curve_process_parameter_definition_mapper::map(entities);
        },
        lg(),
        "Reading yield curve process parameter definition at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void yield_curve_process_parameter_definition_repository::remove(context ctx,
                                                                 const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing yield curve process parameter definition. "
                               << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<yield_curve_process_parameter_definition_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(
        ctx, query, lg(), "Removing yield curve process parameter definition from database.");
}

std::vector<domain::yield_curve_process_parameter_definition>
yield_curve_process_parameter_definition_repository::read_latest(context ctx,
                                                                 std::uint32_t offset,
                                                                 std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading latest yield curve process parameter definitions with offset: " << offset
        << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<yield_curve_process_parameter_definition_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<yield_curve_process_parameter_definition_entity,
                              domain::yield_curve_process_parameter_definition>(
        ctx,
        query,
        [](const auto& entities) {
            return yield_curve_process_parameter_definition_mapper::map(entities);
        },
        lg(),
        "Reading latest yield curve process parameter definitions with pagination.");
}

std::uint32_t
yield_curve_process_parameter_definition_repository::get_total_parameter_definition_count(
    context ctx) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active yield curve process parameter definition count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<yield_curve_process_parameter_definition_entity>(
                           sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active yield curve process parameter definition count: "
                               << count;
    return count;
}

void yield_curve_process_parameter_definition_repository::remove(
    context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<yield_curve_process_parameter_definition_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(
        ctx, query, lg(), "Batch removing yield curve process parameter definitions.");
}


}
