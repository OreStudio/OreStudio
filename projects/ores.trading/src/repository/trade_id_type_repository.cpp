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
#include "ores.trading/repository/trade_id_type_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.trading/domain/trade_id_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading/repository/trade_id_type_entity.hpp"
#include "ores.trading/repository/trade_id_type_mapper.hpp"

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string trade_id_type_repository::sql() {
    return generate_create_table_sql<trade_id_type_entity>(lg());
}

void trade_id_type_repository::write(context ctx, const domain::trade_id_type& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade ID type: " << v.code;
    execute_write_query(ctx, trade_id_type_mapper::map(v),
        lg(), "Writing trade ID type to database.");
}

void trade_id_type_repository::write(
    context ctx, const std::vector<domain::trade_id_type>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade ID types. Count: " << v.size();
    execute_write_query(ctx, trade_id_type_mapper::map(v),
        lg(), "Writing trade ID types to database.");
}

std::vector<domain::trade_id_type>
trade_id_type_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_id_type_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("code"_c);

    return execute_read_query<trade_id_type_entity, domain::trade_id_type>(
        ctx, query,
        [](const auto& entities) { return trade_id_type_mapper::map(entities); },
        lg(), "Reading latest trade ID types");
}

std::vector<domain::trade_id_type>
trade_id_type_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade ID type. code: " << code;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_id_type_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<trade_id_type_entity, domain::trade_id_type>(
        ctx, query,
        [](const auto& entities) { return trade_id_type_mapper::map(entities); },
        lg(), "Reading latest trade ID type by code.");
}

std::vector<domain::trade_id_type>
trade_id_type_repository::read_all(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all trade ID type versions. code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_id_type_entity>> |
        where("tenant_id"_c == tid && "code"_c == code) |
        order_by("version"_c.desc());

    return execute_read_query<trade_id_type_entity, domain::trade_id_type>(
        ctx, query,
        [](const auto& entities) { return trade_id_type_mapper::map(entities); },
        lg(), "Reading all trade ID type versions by code.");
}

void trade_id_type_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade ID type: " << code;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<trade_id_type_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing trade ID type from database.");
}

}
