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
#include "ores.dq.core/repository/code_domain_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.dq.api/domain/code_domain_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/code_domain_entity.hpp"
#include "ores.dq.core/repository/code_domain_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string code_domain_repository::sql() {
    return generate_create_table_sql<code_domain_entity>(lg());
}

void code_domain_repository::write(context ctx, const domain::code_domain& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing code domain: " << v.code;
    execute_write_query(ctx, code_domain_mapper::map(v), lg(), "Writing code domain to database.");
}

void code_domain_repository::write(context ctx, const std::vector<domain::code_domain>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing code domains. Count: " << v.size();
    execute_write_query(ctx, code_domain_mapper::map(v), lg(), "Writing code domains to database.");
}

std::vector<domain::code_domain> code_domain_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<code_domain_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c);

    return execute_read_query<code_domain_entity, domain::code_domain>(
        ctx,
        query,
        [](const auto& entities) { return code_domain_mapper::map(entities); },
        lg(),
        "Reading latest code domains");
}

std::vector<domain::code_domain> code_domain_repository::read_latest(context ctx,
                                                                     const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest code domain. code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<code_domain_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<code_domain_entity, domain::code_domain>(
        ctx,
        query,
        [](const auto& entities) { return code_domain_mapper::map(entities); },
        lg(),
        "Reading latest code domain by code.");
}

std::vector<domain::code_domain> code_domain_repository::read_all(context ctx,
                                                                  const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all code domain versions. code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<code_domain_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<code_domain_entity, domain::code_domain>(
        ctx,
        query,
        [](const auto& entities) { return code_domain_mapper::map(entities); },
        lg(),
        "Reading all code domain versions by code.");
}

std::optional<domain::code_domain> code_domain_repository::read_at_version(context ctx,
                                                                           const std::string& code,
                                                                           std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading code domain at version. code: " << code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<code_domain_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<code_domain_entity, domain::code_domain>(
        ctx,
        query,
        [](const auto& entities) { return code_domain_mapper::map(entities); },
        lg(),
        "Reading code domain at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void code_domain_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing code domain: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<code_domain_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing code domain from database.");
}

std::vector<domain::code_domain>
code_domain_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest code domains with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<code_domain_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<code_domain_entity, domain::code_domain>(
        ctx,
        query,
        [](const auto& entities) { return code_domain_mapper::map(entities); },
        lg(),
        "Reading latest code domains with pagination.");
}

std::uint32_t code_domain_repository::get_total_domain_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active code domain count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<code_domain_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active code domain count: " << count;
    return count;
}

void code_domain_repository::remove(context ctx, const std::vector<std::string>& codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<code_domain_entity> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing code domains.");
}


}
