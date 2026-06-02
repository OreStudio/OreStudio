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
#include "ores.trading.core/repository/activity_type_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.trading.api/domain/activity_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/activity_type_entity.hpp"
#include "ores.trading.core/repository/activity_type_mapper.hpp"

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string activity_type_repository::sql() {
    return generate_create_table_sql<activity_type_entity>(lg());
}

void activity_type_repository::write(context ctx, const domain::activity_type& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing activity type: " << v.code;
    execute_write_query(ctx, activity_type_mapper::map(v),
        lg(), "Writing activity type to database.");
}

void activity_type_repository::write(
    context ctx, const std::vector<domain::activity_type>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing activity types. Count: " << v.size();
    execute_write_query(ctx, activity_type_mapper::map(v),
        lg(), "Writing activity types to database.");
}

std::vector<domain::activity_type>
activity_type_repository::read_latest(context ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<activity_type_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("code"_c);

    return execute_read_query<activity_type_entity, domain::activity_type>(
        ctx, query,
        [](const auto& entities) { return activity_type_mapper::map(entities); },
        lg(), "Reading latest activity types");
}

std::vector<domain::activity_type>
activity_type_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest activity type. code: " << code;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<activity_type_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<activity_type_entity, domain::activity_type>(
        ctx, query,
        [](const auto& entities) { return activity_type_mapper::map(entities); },
        lg(), "Reading latest activity type by code.");
}

std::vector<domain::activity_type>
activity_type_repository::read_all(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all activity type versions. code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<activity_type_entity>> |
        where("tenant_id"_c == tid && "code"_c == code) |
        order_by("version"_c.desc());

    return execute_read_query<activity_type_entity, domain::activity_type>(
        ctx, query,
        [](const auto& entities) { return activity_type_mapper::map(entities); },
        lg(), "Reading all activity type versions by code.");
}

void activity_type_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing activity type: " << code;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<activity_type_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing activity type from database.");
}

void activity_type_repository::remove(
    context ctx, const std::vector<std::string>& codes) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<activity_type_entity> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "batch removing activity_types");
}

}
