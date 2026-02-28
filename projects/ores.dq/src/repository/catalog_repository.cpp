/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.dq/repository/catalog_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/domain/catalog_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/repository/catalog_entity.hpp"
#include "ores.dq/repository/catalog_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string catalog_repository::sql() {
    return generate_create_table_sql<catalog_entity>(lg());
}

catalog_repository::catalog_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void catalog_repository::write(const domain::catalog& catalog) {
    BOOST_LOG_SEV(lg(), debug) << "Writing catalog to database: "
                               << catalog.name;

    execute_write_query(ctx_, catalog_mapper::map(catalog),
        lg(), "writing catalog to database");
}

void catalog_repository::write(
    const std::vector<domain::catalog>& catalogs) {
    BOOST_LOG_SEV(lg(), debug) << "Writing catalogs to database. Count: "
                               << catalogs.size();

    execute_write_query(ctx_, catalog_mapper::map(catalogs),
        lg(), "writing catalogs to database");
}

std::vector<domain::catalog>
catalog_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<catalog_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<catalog_entity, domain::catalog>(
        ctx_, query,
        [](const auto& entities) { return catalog_mapper::map(entities); },
        lg(), "Reading latest catalogs");
}

std::vector<domain::catalog>
catalog_repository::read_latest(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest catalog. Name: " << name;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<catalog_entity>> |
        where("name"_c == name && "valid_to"_c == max.value());

    return execute_read_query<catalog_entity, domain::catalog>(
        ctx_, query,
        [](const auto& entities) { return catalog_mapper::map(entities); },
        lg(), "Reading latest catalog by name.");
}

std::vector<domain::catalog>
catalog_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest catalogs with offset: "
                               << offset << " and limit: " << limit;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<catalog_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<catalog_entity, domain::catalog>(
        ctx_, query,
        [](const auto& entities) { return catalog_mapper::map(entities); },
        lg(), "Reading latest catalogs with pagination.");
}

std::uint32_t catalog_repository::get_total_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active catalog count";

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<catalog_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active catalog count: " << count;
    return count;
}

std::vector<domain::catalog>
catalog_repository::read_all(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all catalog versions. Name: "
                               << name;

    const auto query = sqlgen::read<std::vector<catalog_entity>> |
        where("name"_c == name) |
        order_by("version"_c.desc());

    return execute_read_query<catalog_entity, domain::catalog>(
        ctx_, query,
        [](const auto& entities) { return catalog_mapper::map(entities); },
        lg(), "Reading all catalog versions by name.");
}

void catalog_repository::remove(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing catalog from database: " << name;

    const auto query = sqlgen::delete_from<catalog_entity> |
        where("name"_c == name);

    execute_delete_query(ctx_, query, lg(), "removing catalog from database");
}

}
