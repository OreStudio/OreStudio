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
#include "ores.dq/repository/data_domain_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/domain/data_domain_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/repository/data_domain_entity.hpp"
#include "ores.dq/repository/data_domain_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string data_domain_repository::sql() {
    return generate_create_table_sql<data_domain_entity>(lg());
}

data_domain_repository::data_domain_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void data_domain_repository::write(const domain::data_domain& data_domain) {
    BOOST_LOG_SEV(lg(), debug) << "Writing data_domain to database: "
                               << data_domain.name;

    execute_write_query(ctx_, data_domain_mapper::map(data_domain),
        lg(), "writing data_domain to database");
}

void data_domain_repository::write(
    const std::vector<domain::data_domain>& data_domains) {
    BOOST_LOG_SEV(lg(), debug) << "Writing data_domains to database. Count: "
                               << data_domains.size();

    execute_write_query(ctx_, data_domain_mapper::map(data_domains),
        lg(), "writing data_domains to database");
}

std::vector<domain::data_domain>
data_domain_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<data_domain_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<data_domain_entity, domain::data_domain>(
        ctx_, query,
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(), "Reading latest data_domains");
}

std::vector<domain::data_domain>
data_domain_repository::read_latest(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest data_domain. Name: " << name;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<data_domain_entity>> |
        where("name"_c == name && "valid_to"_c == max.value());

    return execute_read_query<data_domain_entity, domain::data_domain>(
        ctx_, query,
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(), "Reading latest data_domain by name.");
}

std::vector<domain::data_domain>
data_domain_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest data_domains with offset: "
                               << offset << " and limit: " << limit;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<data_domain_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<data_domain_entity, domain::data_domain>(
        ctx_, query,
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(), "Reading latest data_domains with pagination.");
}

std::uint32_t data_domain_repository::get_total_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active data_domain count";

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<data_domain_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active data_domain count: " << count;
    return count;
}

std::vector<domain::data_domain>
data_domain_repository::read_all(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all data_domain versions. Name: "
                               << name;

    const auto query = sqlgen::read<std::vector<data_domain_entity>> |
        where("name"_c == name) |
        order_by("version"_c.desc());

    return execute_read_query<data_domain_entity, domain::data_domain>(
        ctx_, query,
        [](const auto& entities) { return data_domain_mapper::map(entities); },
        lg(), "Reading all data_domain versions by name.");
}

void data_domain_repository::remove(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Removing data_domain from database: " << name;

    const auto query = sqlgen::delete_from<data_domain_entity> |
        where("name"_c == name);

    execute_delete_query(ctx_, query, lg(), "removing data_domain from database");
}

}
