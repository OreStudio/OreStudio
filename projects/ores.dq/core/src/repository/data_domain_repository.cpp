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
#include "ores.dq.core/repository/data_domain_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.dq.api/domain/data_domain_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/data_domain_entity.hpp"
#include "ores.dq.core/repository/data_domain_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string data_domain_repository::sql() {
    return generate_create_table_sql<data_domain_entity>(lg());
}

void data_domain_repository::write(context ctx, const domain::data_domain& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing data domain: " << v.name;
    execute_write_query(ctx, data_domain_mapper::map(v), lg(), "Writing data domain to database.");
}

void data_domain_repository::write(context ctx, const std::vector<domain::data_domain>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing data domains. Count: " << v.size();
    execute_write_query(ctx, data_domain_mapper::map(v), lg(), "Writing data domains to database.");
}

std::vector<domain::data_domain> data_domain_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<data_domain_entity>> |
                       where("valid_to"_c == max.value()) | order_by("name"_c);

    return execute_read_query<data_domain_entity, domain::data_domain>(
        ctx,
        query,
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(),
        "Reading latest data domains");
}

std::vector<domain::data_domain> data_domain_repository::read_latest(context ctx,
                                                                     const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest data domain. name: " << name;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<data_domain_entity>> |
                       where("name"_c == name && "valid_to"_c == max.value());

    return execute_read_query<data_domain_entity, domain::data_domain>(
        ctx,
        query,
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(),
        "Reading latest data domain by name.");
}

std::vector<domain::data_domain> data_domain_repository::read_all(context ctx,
                                                                  const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all data domain versions. name: " << name;
    const auto query = sqlgen::read<std::vector<data_domain_entity>> | where("name"_c == name) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<data_domain_entity, domain::data_domain>(
        ctx,
        query,
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(),
        "Reading all data domain versions by name.");
}

std::optional<domain::data_domain> data_domain_repository::read_at_version(context ctx,
                                                                           const std::string& name,
                                                                           std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading data domain at version. name: " << name
                               << " version: " << version;
    const auto query = sqlgen::read<std::vector<data_domain_entity>> |
                       where("name"_c == name && "version"_c == version) | sqlgen::limit(1);

    const auto entities = execute_read_query<data_domain_entity, domain::data_domain>(
        ctx,
        query,
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(),
        "Reading data domain at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void data_domain_repository::remove(context ctx, const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing data domain: " << name;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<data_domain_entity> |
        where("tenant_id"_c == tid && "name"_c == name && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing data domain from database.");
}

std::vector<domain::data_domain>
data_domain_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest data domains with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<data_domain_entity>> |
                       where("valid_to"_c == max.value()) | order_by("name"_c) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<data_domain_entity, domain::data_domain>(
        ctx,
        query,
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(),
        "Reading latest data domains with pagination.");
}

std::uint32_t data_domain_repository::get_total_domain_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active data domain count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<data_domain_entity>(sqlgen::count().as<"count">()) |
                       where("valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active data domain count: " << count;
    return count;
}

void data_domain_repository::remove(context ctx, const std::vector<std::string>& names) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<data_domain_entity> |
        where("tenant_id"_c == tid && "name"_c.in(names) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing data domains.");
}


}
