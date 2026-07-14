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
#include "ores.refdata.core/repository/instrument_code_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/instrument_code_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/instrument_code_entity.hpp"
#include "ores.refdata.core/repository/instrument_code_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string instrument_code_repository::sql() {
    return generate_create_table_sql<instrument_code_entity>(lg());
}

void instrument_code_repository::write(context ctx, const domain::instrument_code& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing instrument code: " << v.code;
    execute_write_query(
        ctx, instrument_code_mapper::map(v), lg(), "Writing instrument code to database.");
}

void instrument_code_repository::write(context ctx, const std::vector<domain::instrument_code>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing instrument codes. Count: " << v.size();
    execute_write_query(
        ctx, instrument_code_mapper::map(v), lg(), "Writing instrument codes to database.");
}

std::vector<domain::instrument_code> instrument_code_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_code_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c);

    return execute_read_query<instrument_code_entity, domain::instrument_code>(
        ctx,
        query,
        [](const auto& entities) { return instrument_code_mapper::map(entities); },
        lg(),
        "Reading latest instrument codes");
}

std::vector<domain::instrument_code>
instrument_code_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest instrument code. code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<instrument_code_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<instrument_code_entity, domain::instrument_code>(
        ctx,
        query,
        [](const auto& entities) { return instrument_code_mapper::map(entities); },
        lg(),
        "Reading latest instrument code by code.");
}

std::vector<domain::instrument_code> instrument_code_repository::read_all(context ctx,
                                                                          const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all instrument code versions. code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_code_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<instrument_code_entity, domain::instrument_code>(
        ctx,
        query,
        [](const auto& entities) { return instrument_code_mapper::map(entities); },
        lg(),
        "Reading all instrument code versions by code.");
}

std::optional<domain::instrument_code> instrument_code_repository::read_at_version(
    context ctx, const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading instrument code at version. code: " << code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_code_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<instrument_code_entity, domain::instrument_code>(
        ctx,
        query,
        [](const auto& entities) { return instrument_code_mapper::map(entities); },
        lg(),
        "Reading instrument code at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void instrument_code_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing instrument code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<instrument_code_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing instrument code from database.");
}

std::vector<domain::instrument_code>
instrument_code_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest instrument codes with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<instrument_code_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<instrument_code_entity, domain::instrument_code>(
        ctx,
        query,
        [](const auto& entities) { return instrument_code_mapper::map(entities); },
        lg(),
        "Reading latest instrument codes with pagination.");
}

std::uint32_t instrument_code_repository::get_total_instrument_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active instrument code count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<instrument_code_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active instrument code count: " << count;
    return count;
}

void instrument_code_repository::remove(context ctx, const std::vector<std::string>& codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<instrument_code_entity> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing instrument codes.");
}


}
