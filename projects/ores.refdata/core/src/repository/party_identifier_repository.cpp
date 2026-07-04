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
#include "ores.refdata.core/repository/party_identifier_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/party_identifier_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/party_identifier_entity.hpp"
#include "ores.refdata.core/repository/party_identifier_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string party_identifier_repository::sql() {
    return generate_create_table_sql<party_identifier_entity>(lg());
}

void party_identifier_repository::write(context ctx, const domain::party_identifier& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party identifier: " << v.id;
    execute_write_query(
        ctx, party_identifier_mapper::map(v), lg(), "Writing party identifier to database.");
}

void party_identifier_repository::write(context ctx,
                                        const std::vector<domain::party_identifier>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing party identifiers. Count: " << v.size();
    execute_write_query(
        ctx, party_identifier_mapper::map(v), lg(), "Writing party identifiers to database.");
}

std::vector<domain::party_identifier> party_identifier_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_identifier_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<party_identifier_entity, domain::party_identifier>(
        ctx,
        query,
        [](const auto& entities) { return party_identifier_mapper::map(entities); },
        lg(),
        "Reading latest party identifiers");
}

std::vector<domain::party_identifier>
party_identifier_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party identifier. id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_identifier_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<party_identifier_entity, domain::party_identifier>(
        ctx,
        query,
        [](const auto& entities) { return party_identifier_mapper::map(entities); },
        lg(),
        "Reading latest party identifier by id.");
}

std::vector<domain::party_identifier> party_identifier_repository::read_all(context ctx,
                                                                            const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all party identifier versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_identifier_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) | order_by("version"_c.desc());

    return execute_read_query<party_identifier_entity, domain::party_identifier>(
        ctx,
        query,
        [](const auto& entities) { return party_identifier_mapper::map(entities); },
        lg(),
        "Reading all party identifier versions by id.");
}

std::vector<domain::party_identifier> party_identifier_repository::read_latest_by_party_id(
    context ctx, const std::string& party_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party identifiers. party_id: " << party_id
                               << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<party_identifier_entity>> |
        where("tenant_id"_c == tid && "party_id"_c == party_id && "valid_to"_c == max.value()) |
        order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<party_identifier_entity, domain::party_identifier>(
        ctx,
        query,
        [](const auto& entities) { return party_identifier_mapper::map(entities); },
        lg(),
        "Reading latest party identifiers by party_id.");
}

std::uint32_t party_identifier_repository::get_total_party_identifier_count_by_party_id(
    context ctx, const std::string& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active party identifiers count. party_id: "
                               << party_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<party_identifier_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "party_id"_c == party_id && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active party identifiers count by party_id: " << count;
    return count;
}

void party_identifier_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing party identifier: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<party_identifier_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing party identifier from database.");
}

std::vector<domain::party_identifier>
party_identifier_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest party identifiers with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<party_identifier_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<party_identifier_entity, domain::party_identifier>(
        ctx,
        query,
        [](const auto& entities) { return party_identifier_mapper::map(entities); },
        lg(),
        "Reading latest party identifiers with pagination.");
}

std::uint32_t party_identifier_repository::get_total_party_identifier_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active party identifier count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<party_identifier_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active party identifier count: " << count;
    return count;
}

void party_identifier_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<party_identifier_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing party identifiers.");
}


}
