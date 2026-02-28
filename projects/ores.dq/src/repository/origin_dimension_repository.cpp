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
#include "ores.dq/repository/origin_dimension_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/domain/origin_dimension_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/repository/origin_dimension_entity.hpp"
#include "ores.dq/repository/origin_dimension_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string origin_dimension_repository::sql() {
    return generate_create_table_sql<origin_dimension_entity>(lg());
}

origin_dimension_repository::origin_dimension_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void origin_dimension_repository::write(const domain::origin_dimension& dimension) {
    BOOST_LOG_SEV(lg(), debug) << "Writing origin_dimension to database: "
                               << dimension.code;
    execute_write_query(ctx_, origin_dimension_mapper::map(dimension),
        lg(), "writing origin_dimension to database");
}

void origin_dimension_repository::write(
    const std::vector<domain::origin_dimension>& dimensions) {
    BOOST_LOG_SEV(lg(), debug) << "Writing origin_dimensions to database. Count: "
                               << dimensions.size();
    execute_write_query(ctx_, origin_dimension_mapper::map(dimensions),
        lg(), "writing origin_dimensions to database");
}

std::vector<domain::origin_dimension>
origin_dimension_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<origin_dimension_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("code"_c);

    return execute_read_query<origin_dimension_entity, domain::origin_dimension>(
        ctx_, query,
        [](const auto& entities) { return origin_dimension_mapper::map(entities); },
        lg(), "Reading latest origin_dimensions");
}

std::vector<domain::origin_dimension>
origin_dimension_repository::read_latest(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest origin_dimension. Code: " << code;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<origin_dimension_entity>> |
        where("code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<origin_dimension_entity, domain::origin_dimension>(
        ctx_, query,
        [](const auto& entities) { return origin_dimension_mapper::map(entities); },
        lg(), "Reading latest origin_dimension by code.");
}

std::vector<domain::origin_dimension>
origin_dimension_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest origin_dimensions with offset: "
                               << offset << " and limit: " << limit;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<origin_dimension_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("code"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<origin_dimension_entity, domain::origin_dimension>(
        ctx_, query,
        [](const auto& entities) { return origin_dimension_mapper::map(entities); },
        lg(), "Reading latest origin_dimensions with pagination.");
}

std::uint32_t origin_dimension_repository::get_total_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active origin_dimension count";

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<origin_dimension_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active origin_dimension count: " << count;
    return count;
}

std::vector<domain::origin_dimension>
origin_dimension_repository::read_all(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all origin_dimension versions. Code: " << code;

    const auto query = sqlgen::read<std::vector<origin_dimension_entity>> |
        where("code"_c == code) |
        order_by("version"_c.desc());

    return execute_read_query<origin_dimension_entity, domain::origin_dimension>(
        ctx_, query,
        [](const auto& entities) { return origin_dimension_mapper::map(entities); },
        lg(), "Reading all origin_dimension versions by code.");
}

void origin_dimension_repository::remove(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing origin_dimension from database: " << code;

    const auto query = sqlgen::delete_from<origin_dimension_entity> |
        where("code"_c == code);

    execute_delete_query(ctx_, query, lg(), "removing origin_dimension from database");
}

void origin_dimension_repository::remove(const std::vector<std::string>& codes) {
    const auto query = sqlgen::delete_from<origin_dimension_entity> |
        where("code"_c.in(codes));
    execute_delete_query(ctx_, query, lg(), "batch removing origin_dimensions");
}

}
