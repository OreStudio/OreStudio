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
#include "ores.refdata.core/repository/book_status_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/book_status_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/book_status_entity.hpp"
#include "ores.refdata.core/repository/book_status_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string book_status_repository::sql() {
    return generate_create_table_sql<book_status_entity>(lg());
}

void book_status_repository::write(context ctx, const domain::book_status& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing book status: " << v.code;
    execute_write_query(ctx, book_status_mapper::map(v), lg(), "Writing book status to database.");
}

void book_status_repository::write(context ctx, const std::vector<domain::book_status>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing book statuses. Count: " << v.size();
    execute_write_query(
        ctx, book_status_mapper::map(v), lg(), "Writing book statuses to database.");
}

std::vector<domain::book_status> book_status_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<book_status_entity>> |
                       where("valid_to"_c == max.value()) | order_by("code"_c);

    return execute_read_query<book_status_entity, domain::book_status>(
        ctx,
        query,
        [](const auto& entities) { return book_status_mapper::map(entities); },
        lg(),
        "Reading latest book statuses");
}

std::vector<domain::book_status> book_status_repository::read_latest(context ctx,
                                                                     const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest book status. code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<book_status_entity>> |
                       where("code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<book_status_entity, domain::book_status>(
        ctx,
        query,
        [](const auto& entities) { return book_status_mapper::map(entities); },
        lg(),
        "Reading latest book status by code.");
}

std::vector<domain::book_status> book_status_repository::read_all(context ctx,
                                                                  const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all book status versions. code: " << code;
    const auto query = sqlgen::read<std::vector<book_status_entity>> | where("code"_c == code) |
                       order_by("version"_c.desc());

    return execute_read_query<book_status_entity, domain::book_status>(
        ctx,
        query,
        [](const auto& entities) { return book_status_mapper::map(entities); },
        lg(),
        "Reading all book status versions by code.");
}

void book_status_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing book status: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::delete_from<book_status_entity> |
                       where("code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing book status from database.");
}

void book_status_repository::remove(context ctx, const std::vector<std::string>& codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::delete_from<book_status_entity> |
                       where("code"_c.in(codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "batch removing book statuses");
}


}
