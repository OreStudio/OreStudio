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
#include "ores.reporting/repository/concurrency_policy_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.reporting/domain/concurrency_policy_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting/repository/concurrency_policy_entity.hpp"
#include "ores.reporting/repository/concurrency_policy_mapper.hpp"

namespace ores::reporting::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string concurrency_policy_repository::sql() {
    return generate_create_table_sql<concurrency_policy_entity>(lg());
}

void concurrency_policy_repository::write(context ctx, const domain::concurrency_policy& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing concurrency policy: " << v.code;
    execute_write_query(ctx, concurrency_policy_mapper::map(v),
        lg(), "Writing concurrency policy to database.");
}

void concurrency_policy_repository::write(
    context ctx, const std::vector<domain::concurrency_policy>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing concurrency policies. Count: " << v.size();
    execute_write_query(ctx, concurrency_policy_mapper::map(v),
        lg(), "Writing concurrency policies to database.");
}

std::vector<domain::concurrency_policy>
concurrency_policy_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<concurrency_policy_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("code"_c);

    return execute_read_query<concurrency_policy_entity, domain::concurrency_policy>(
        ctx, query,
        [](const auto& entities) { return concurrency_policy_mapper::map(entities); },
        lg(), "Reading latest concurrency policies");
}

std::vector<domain::concurrency_policy>
concurrency_policy_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest concurrency policy. code: " << code;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<concurrency_policy_entity>> |
        where("code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<concurrency_policy_entity, domain::concurrency_policy>(
        ctx, query,
        [](const auto& entities) { return concurrency_policy_mapper::map(entities); },
        lg(), "Reading latest concurrency policy by code.");
}

std::vector<domain::concurrency_policy>
concurrency_policy_repository::read_all(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all concurrency policy versions. code: " << code;
    const auto query = sqlgen::read<std::vector<concurrency_policy_entity>> |
        where("code"_c == code) |
        order_by("version"_c.desc());

    return execute_read_query<concurrency_policy_entity, domain::concurrency_policy>(
        ctx, query,
        [](const auto& entities) { return concurrency_policy_mapper::map(entities); },
        lg(), "Reading all concurrency policy versions by code.");
}

void concurrency_policy_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing concurrency policy: " << code;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::delete_from<concurrency_policy_entity> |
        where("code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing concurrency policy from database.");
}

}
