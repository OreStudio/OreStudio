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
#include "ores.marketdata.core/repository/feed_binding_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.marketdata.api/domain/feed_binding_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/feed_binding_entity.hpp"
#include "ores.marketdata.core/repository/feed_binding_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::marketdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string feed_binding_repository::sql() {
    return generate_create_table_sql<feed_binding_entity>(lg());
}

void feed_binding_repository::write(context ctx, const domain::feed_binding& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing feed binding: " << v.id;
    execute_write_query(
        ctx, feed_binding_mapper::map(v), lg(), "Writing feed binding to database.");
}

void feed_binding_repository::write(context ctx, const std::vector<domain::feed_binding>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing feed bindings. Count: " << v.size();
    execute_write_query(
        ctx, feed_binding_mapper::map(v), lg(), "Writing feed bindings to database.");
}

std::vector<domain::feed_binding> feed_binding_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<feed_binding_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<feed_binding_entity, domain::feed_binding>(
        ctx,
        query,
        [](const auto& entities) { return feed_binding_mapper::map(entities); },
        lg(),
        "Reading latest feed bindings");
}

std::vector<domain::feed_binding> feed_binding_repository::read_latest(context ctx,
                                                                       const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest feed binding. id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<feed_binding_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<feed_binding_entity, domain::feed_binding>(
        ctx,
        query,
        [](const auto& entities) { return feed_binding_mapper::map(entities); },
        lg(),
        "Reading latest feed binding by id.");
}

std::vector<domain::feed_binding> feed_binding_repository::read_all(context ctx,
                                                                    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all feed binding versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<feed_binding_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) | order_by("version"_c.desc());

    return execute_read_query<feed_binding_entity, domain::feed_binding>(
        ctx,
        query,
        [](const auto& entities) { return feed_binding_mapper::map(entities); },
        lg(),
        "Reading all feed binding versions by id.");
}

void feed_binding_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing feed binding: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<feed_binding_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing feed binding from database.");
}

std::vector<domain::feed_binding>
feed_binding_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest feed bindings with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<feed_binding_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<feed_binding_entity, domain::feed_binding>(
        ctx,
        query,
        [](const auto& entities) { return feed_binding_mapper::map(entities); },
        lg(),
        "Reading latest feed bindings with pagination.");
}

std::uint32_t feed_binding_repository::get_total_feed_binding_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active feed binding count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<feed_binding_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active feed binding count: " << count;
    return count;
}

void feed_binding_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<feed_binding_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing feed bindings.");
}


std::vector<domain::feed_binding> feed_binding_repository::read_latest_all_tenants(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<feed_binding_entity>> |
                       where("valid_to"_c == max.value()) | order_by("id"_c);

    return execute_read_query<feed_binding_entity, domain::feed_binding>(
        ctx,
        query,
        [](const auto& entities) { return feed_binding_mapper::map(entities); },
        lg(),
        "Reading latest feed bindings across all tenants");
}

}
