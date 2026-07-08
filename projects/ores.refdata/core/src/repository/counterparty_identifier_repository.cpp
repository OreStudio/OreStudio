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
#include "ores.refdata.core/repository/counterparty_identifier_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/counterparty_identifier_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/counterparty_identifier_entity.hpp"
#include "ores.refdata.core/repository/counterparty_identifier_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string counterparty_identifier_repository::sql() {
    return generate_create_table_sql<counterparty_identifier_entity>(lg());
}

void counterparty_identifier_repository::write(context ctx,
                                               const domain::counterparty_identifier& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparty identifier: " << v.id;
    execute_write_query(ctx,
                        counterparty_identifier_mapper::map(v),
                        lg(),
                        "Writing counterparty identifier to database.");
}

void counterparty_identifier_repository::write(
    context ctx, const std::vector<domain::counterparty_identifier>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparty identifiers. Count: " << v.size();
    execute_write_query(ctx,
                        counterparty_identifier_mapper::map(v),
                        lg(),
                        "Writing counterparty identifiers to database.");
}

std::vector<domain::counterparty_identifier>
counterparty_identifier_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_identifier_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<counterparty_identifier_entity, domain::counterparty_identifier>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_identifier_mapper::map(entities); },
        lg(),
        "Reading latest counterparty identifiers");
}

std::vector<domain::counterparty_identifier>
counterparty_identifier_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty identifier. id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_identifier_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<counterparty_identifier_entity, domain::counterparty_identifier>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_identifier_mapper::map(entities); },
        lg(),
        "Reading latest counterparty identifier by id.");
}

std::vector<domain::counterparty_identifier>
counterparty_identifier_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all counterparty identifier versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_identifier_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) | order_by("version"_c.desc());

    return execute_read_query<counterparty_identifier_entity, domain::counterparty_identifier>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_identifier_mapper::map(entities); },
        lg(),
        "Reading all counterparty identifier versions by id.");
}

std::optional<domain::counterparty_identifier> counterparty_identifier_repository::read_at_version(
    context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading counterparty identifier at version. id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_identifier_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities =
        execute_read_query<counterparty_identifier_entity, domain::counterparty_identifier>(
            ctx,
            query,
            [](const auto& entities) { return counterparty_identifier_mapper::map(entities); },
            lg(),
            "Reading counterparty identifier at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::counterparty_identifier>
counterparty_identifier_repository::read_latest_by_counterparty_id(
    context ctx, const std::string& counterparty_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty identifiers. counterparty_id: "
                               << counterparty_id << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_identifier_entity>> |
                       where("tenant_id"_c == tid && "counterparty_id"_c == counterparty_id &&
                             "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<counterparty_identifier_entity, domain::counterparty_identifier>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_identifier_mapper::map(entities); },
        lg(),
        "Reading latest counterparty identifiers by counterparty_id.");
}

std::uint32_t
counterparty_identifier_repository::get_total_counterparty_identifier_count_by_counterparty_id(
    context ctx, const std::string& counterparty_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active counterparty identifiers count. counterparty_id: "
        << counterparty_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<counterparty_identifier_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "counterparty_id"_c == counterparty_id &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active counterparty identifiers count by counterparty_id: "
                               << count;
    return count;
}
std::vector<domain::counterparty_identifier>
counterparty_identifier_repository::read_by_counterparty_id_as_of(
    context ctx,
    const std::string& counterparty_id,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug) << "Reading counterparty identifiers as of window. counterparty_id: "
                               << counterparty_id;

    const auto vf(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_from_bound), lg()));
    const auto vt(
        make_timestamp(ores::platform::time::datetime::to_db_string(valid_to_bound), lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_identifier_entity>> |
                       where("tenant_id"_c == tid && "counterparty_id"_c == counterparty_id &&
                             "valid_from"_c < vt.value() && "valid_to"_c > vf.value()) |
                       order_by("id"_c);

    return execute_read_query<counterparty_identifier_entity, domain::counterparty_identifier>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_identifier_mapper::map(entities); },
        lg(),
        "Reading counterparty identifiers as of window by counterparty_id.");
}

void counterparty_identifier_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty identifier: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<counterparty_identifier_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing counterparty identifier from database.");
}

std::vector<domain::counterparty_identifier> counterparty_identifier_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty identifiers with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_identifier_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<counterparty_identifier_entity, domain::counterparty_identifier>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_identifier_mapper::map(entities); },
        lg(),
        "Reading latest counterparty identifiers with pagination.");
}

std::uint32_t
counterparty_identifier_repository::get_total_counterparty_identifier_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active counterparty identifier count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<counterparty_identifier_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active counterparty identifier count: " << count;
    return count;
}

void counterparty_identifier_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<counterparty_identifier_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing counterparty identifiers.");
}


}
