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
#include "ores.iam.core/repository/account_type_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/account_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/account_type_entity.hpp"
#include "ores.iam.core/repository/account_type_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string account_type_repository::sql() {
    return generate_create_table_sql<account_type_entity>(lg());
}

void account_type_repository::write(context ctx, const domain::account_type& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account type. " << "type: " << v.type;
    execute_write_query(
        ctx, account_type_mapper::map(v), lg(), "Writing account type to database.");
}

void account_type_repository::write(context ctx, const std::vector<domain::account_type>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account types. Count: " << v.size();
    execute_write_query(
        ctx, account_type_mapper::map(v), lg(), "Writing account types to database.");
}

std::vector<domain::account_type> account_type_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_type_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("type"_c);

    return execute_read_query<account_type_entity, domain::account_type>(
        ctx,
        query,
        [](const auto& entities) { return account_type_mapper::map(entities); },
        lg(),
        "Reading latest account types");
}

std::vector<domain::account_type> account_type_repository::read_latest(context ctx,
                                                                       const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account type. " << "type: " << type;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<account_type_entity>> |
        where("tenant_id"_c == tid && "type"_c == type && "valid_to"_c == max.value());

    return execute_read_query<account_type_entity, domain::account_type>(
        ctx,
        query,
        [](const auto& entities) { return account_type_mapper::map(entities); },
        lg(),
        "Reading latest account type by type.");
}


std::vector<domain::account_type> account_type_repository::read_all(context ctx,
                                                                    const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all account type versions. " << "type: " << type;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_type_entity>> |
                       where("tenant_id"_c == tid && "type"_c == type) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<account_type_entity, domain::account_type>(
        ctx,
        query,
        [](const auto& entities) { return account_type_mapper::map(entities); },
        lg(),
        "Reading all account type versions by type.");
}

std::optional<domain::account_type> account_type_repository::read_at_version(
    context ctx, const std::string& type, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading account type at version. " << "type: " << type
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_type_entity>> |
                       where("tenant_id"_c == tid && "type"_c == type && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<account_type_entity, domain::account_type>(
        ctx,
        query,
        [](const auto& entities) { return account_type_mapper::map(entities); },
        lg(),
        "Reading account type at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void account_type_repository::remove(context ctx, const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account type. " << "type: " << type;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<account_type_entity> |
        where("tenant_id"_c == tid && "type"_c == type && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing account type from database.");
}

std::vector<domain::account_type>
account_type_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account types with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_type_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("type"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<account_type_entity, domain::account_type>(
        ctx,
        query,
        [](const auto& entities) { return account_type_mapper::map(entities); },
        lg(),
        "Reading latest account types with pagination.");
}

std::uint32_t account_type_repository::get_total_type_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active account type count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<account_type_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active account type count: " << count;
    return count;
}

void account_type_repository::remove(context ctx, const std::vector<std::string>& types) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<account_type_entity> |
        where("tenant_id"_c == tid && "type"_c.in(types) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing account types.");
}


}
