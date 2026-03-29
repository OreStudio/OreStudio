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
#include "ores.dq/repository/badge_definition_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/domain/badge_definition_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/repository/badge_definition_entity.hpp"
#include "ores.dq/repository/badge_definition_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string badge_definition_repository::sql() {
    return generate_create_table_sql<badge_definition_entity>(lg());
}

void badge_definition_repository::write(context ctx, const domain::badge_definition& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing badge definition: " << v.code;
    execute_write_query(ctx, badge_definition_mapper::map(v),
        lg(), "Writing badge definition to database.");
}

void badge_definition_repository::write(
    context ctx, const std::vector<domain::badge_definition>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing badge definitions. Count: " << v.size();
    execute_write_query(ctx, badge_definition_mapper::map(v),
        lg(), "Writing badge definitions to database.");
}

std::vector<domain::badge_definition>
badge_definition_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_definition_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("code"_c);

    return execute_read_query<badge_definition_entity, domain::badge_definition>(
        ctx, query,
        [](const auto& entities) { return badge_definition_mapper::map(entities); },
        lg(), "Reading latest badge definitions");
}

std::vector<domain::badge_definition>
badge_definition_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest badge definition. code: " << code;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_definition_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<badge_definition_entity, domain::badge_definition>(
        ctx, query,
        [](const auto& entities) { return badge_definition_mapper::map(entities); },
        lg(), "Reading latest badge definition by code.");
}

std::vector<domain::badge_definition>
badge_definition_repository::read_all(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all badge definition versions. code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_definition_entity>> |
        where("tenant_id"_c == tid && "code"_c == code) |
        order_by("version"_c.desc());

    return execute_read_query<badge_definition_entity, domain::badge_definition>(
        ctx, query,
        [](const auto& entities) { return badge_definition_mapper::map(entities); },
        lg(), "Reading all badge definition versions by code.");
}

void badge_definition_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing badge definition: " << code;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<badge_definition_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing badge definition from database.");
}

}
