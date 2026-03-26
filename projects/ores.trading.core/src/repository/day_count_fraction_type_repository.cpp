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
#include "ores.trading.core/repository/day_count_fraction_type_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.trading.api/domain/day_count_fraction_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/day_count_fraction_type_entity.hpp"
#include "ores.trading.core/repository/day_count_fraction_type_mapper.hpp"

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string day_count_fraction_type_repository::sql() {
    return generate_create_table_sql<day_count_fraction_type_entity>(lg());
}

void day_count_fraction_type_repository::write(context ctx, const domain::day_count_fraction_type& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing day count fraction type: " << v.code;
    execute_write_query(ctx, day_count_fraction_type_mapper::map(v),
        lg(), "Writing day count fraction type to database.");
}

void day_count_fraction_type_repository::write(
    context ctx, const std::vector<domain::day_count_fraction_type>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing day count fraction types. Count: " << v.size();
    execute_write_query(ctx, day_count_fraction_type_mapper::map(v),
        lg(), "Writing day count fraction types to database.");
}

std::vector<domain::day_count_fraction_type>
day_count_fraction_type_repository::read_latest(context ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<day_count_fraction_type_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("code"_c);

    return execute_read_query<day_count_fraction_type_entity, domain::day_count_fraction_type>(
        ctx, query,
        [](const auto& entities) { return day_count_fraction_type_mapper::map(entities); },
        lg(), "Reading latest day count fraction types");
}

std::vector<domain::day_count_fraction_type>
day_count_fraction_type_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest day count fraction type. code: " << code;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<day_count_fraction_type_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<day_count_fraction_type_entity, domain::day_count_fraction_type>(
        ctx, query,
        [](const auto& entities) { return day_count_fraction_type_mapper::map(entities); },
        lg(), "Reading latest day count fraction type by code.");
}

std::vector<domain::day_count_fraction_type>
day_count_fraction_type_repository::read_all(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all day count fraction type versions. code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<day_count_fraction_type_entity>> |
        where("tenant_id"_c == tid && "code"_c == code) |
        order_by("version"_c.desc());

    return execute_read_query<day_count_fraction_type_entity, domain::day_count_fraction_type>(
        ctx, query,
        [](const auto& entities) { return day_count_fraction_type_mapper::map(entities); },
        lg(), "Reading all day count fraction type versions by code.");
}

void day_count_fraction_type_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing day count fraction type: " << code;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<day_count_fraction_type_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing day count fraction type from database.");
}
void day_count_fraction_type_repository::remove(
    context ctx, const std::vector<std::string>& codes) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<day_count_fraction_type_entity> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "batch removing day count fraction types");
}

}
