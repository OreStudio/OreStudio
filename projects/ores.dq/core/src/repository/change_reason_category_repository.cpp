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
#include "ores.dq.core/repository/change_reason_category_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.dq.api/domain/change_reason_category_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/change_reason_category_entity.hpp"
#include "ores.dq.core/repository/change_reason_category_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string change_reason_category_repository::sql() {
    return generate_create_table_sql<change_reason_category_entity>(lg());
}

void change_reason_category_repository::write(context ctx,
                                              const domain::change_reason_category& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing change reason category: " << v.code;
    execute_write_query(ctx,
                        change_reason_category_mapper::map(v),
                        lg(),
                        "Writing change reason category to database.");
}

void change_reason_category_repository::write(
    context ctx, const std::vector<domain::change_reason_category>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing change reason categories. Count: " << v.size();
    execute_write_query(ctx,
                        change_reason_category_mapper::map(v),
                        lg(),
                        "Writing change reason categories to database.");
}

std::vector<domain::change_reason_category>
change_reason_category_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<change_reason_category_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c);

    return execute_read_query<change_reason_category_entity, domain::change_reason_category>(
        ctx,
        query,
        [](const auto& entities) { return change_reason_category_mapper::map(entities); },
        lg(),
        "Reading latest change reason categories");
}

std::vector<domain::change_reason_category>
change_reason_category_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest change reason category. code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<change_reason_category_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<change_reason_category_entity, domain::change_reason_category>(
        ctx,
        query,
        [](const auto& entities) { return change_reason_category_mapper::map(entities); },
        lg(),
        "Reading latest change reason category by code.");
}

std::vector<domain::change_reason_category>
change_reason_category_repository::read_all(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all change reason category versions. code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<change_reason_category_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<change_reason_category_entity, domain::change_reason_category>(
        ctx,
        query,
        [](const auto& entities) { return change_reason_category_mapper::map(entities); },
        lg(),
        "Reading all change reason category versions by code.");
}

std::optional<domain::change_reason_category> change_reason_category_repository::read_at_version(
    context ctx, const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading change reason category at version. code: " << code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<change_reason_category_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<change_reason_category_entity, domain::change_reason_category>(
            ctx,
            query,
            [](const auto& entities) { return change_reason_category_mapper::map(entities); },
            lg(),
            "Reading change reason category at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void change_reason_category_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing change reason category: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<change_reason_category_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing change reason category from database.");
}

std::vector<domain::change_reason_category> change_reason_category_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest change reason categories with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<change_reason_category_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<change_reason_category_entity, domain::change_reason_category>(
        ctx,
        query,
        [](const auto& entities) { return change_reason_category_mapper::map(entities); },
        lg(),
        "Reading latest change reason categories with pagination.");
}

std::uint32_t change_reason_category_repository::get_total_category_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active change reason category count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<change_reason_category_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active change reason category count: " << count;
    return count;
}

void change_reason_category_repository::remove(context ctx, const std::vector<std::string>& codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<change_reason_category_entity> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing change reason categories.");
}


}
