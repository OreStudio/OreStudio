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
#include "ores.dq/repository/methodology_repository.hpp"

#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/domain/methodology_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/repository/methodology_entity.hpp"
#include "ores.dq/repository/methodology_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string methodology_repository::sql() {
    return generate_create_table_sql<methodology_entity>(lg());
}

methodology_repository::methodology_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void methodology_repository::write(const domain::methodology& methodology) {
    BOOST_LOG_SEV(lg(), debug) << "Writing methodology to database: "
                               << methodology.id;
    execute_write_query(ctx_, methodology_mapper::map(methodology),
        lg(), "writing methodology to database");
}

void methodology_repository::write(
    const std::vector<domain::methodology>& methodologies) {
    BOOST_LOG_SEV(lg(), debug) << "Writing methodologies to database. Count: "
                               << methodologies.size();
    execute_write_query(ctx_, methodology_mapper::map(methodologies),
        lg(), "writing methodologies to database");
}

std::vector<domain::methodology>
methodology_repository::read_latest() {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<methodology_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<methodology_entity, domain::methodology>(
        ctx_, query,
        [](const auto& entities) { return methodology_mapper::map(entities); },
        lg(), "Reading latest methodologies");
}

std::vector<domain::methodology>
methodology_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest methodology. Id: " << id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<methodology_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<methodology_entity, domain::methodology>(
        ctx_, query,
        [](const auto& entities) { return methodology_mapper::map(entities); },
        lg(), "Reading latest methodology by id.");
}

std::vector<domain::methodology>
methodology_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest methodologies with offset: "
                               << offset << " and limit: " << limit;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<methodology_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<methodology_entity, domain::methodology>(
        ctx_, query,
        [](const auto& entities) { return methodology_mapper::map(entities); },
        lg(), "Reading latest methodologies with pagination.");
}

std::uint32_t methodology_repository::get_total_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active methodology count";

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<methodology_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active methodology count: " << count;
    return count;
}

std::vector<domain::methodology>
methodology_repository::read_all(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all methodology versions. Id: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<methodology_entity>> |
        where("id"_c == id_str) |
        order_by("version"_c.desc());

    return execute_read_query<methodology_entity, domain::methodology>(
        ctx_, query,
        [](const auto& entities) { return methodology_mapper::map(entities); },
        lg(), "Reading all methodology versions by id.");
}

void methodology_repository::remove(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing methodology from database: " << id;

    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::delete_from<methodology_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing methodology from database");
}

}
