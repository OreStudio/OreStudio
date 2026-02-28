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
#include "ores.trading/repository/trade_party_role_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.trading/domain/trade_party_role_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading/repository/trade_party_role_entity.hpp"
#include "ores.trading/repository/trade_party_role_mapper.hpp"

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string trade_party_role_repository::sql() {
    return generate_create_table_sql<trade_party_role_entity>(lg());
}

void trade_party_role_repository::write(context ctx, const domain::trade_party_role& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade party role: " << v.id;
    execute_write_query(ctx, trade_party_role_mapper::map(v),
        lg(), "Writing trade party role to database.");
}

void trade_party_role_repository::write(
    context ctx, const std::vector<domain::trade_party_role>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade party roles. Count: " << v.size();
    execute_write_query(ctx, trade_party_role_mapper::map(v),
        lg(), "Writing trade party roles to database.");
}

std::vector<domain::trade_party_role>
trade_party_role_repository::read_latest(context ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_party_role_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<trade_party_role_entity, domain::trade_party_role>(
        ctx, query,
        [](const auto& entities) { return trade_party_role_mapper::map(entities); },
        lg(), "Reading latest trade party roles");
}

std::vector<domain::trade_party_role>
trade_party_role_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade party role. id: " << id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_party_role_entity>> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<trade_party_role_entity, domain::trade_party_role>(
        ctx, query,
        [](const auto& entities) { return trade_party_role_mapper::map(entities); },
        lg(), "Reading latest trade party role by id.");
}

std::vector<domain::trade_party_role>
trade_party_role_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all trade party role versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_party_role_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<trade_party_role_entity, domain::trade_party_role>(
        ctx, query,
        [](const auto& entities) { return trade_party_role_mapper::map(entities); },
        lg(), "Reading all trade party role versions by id.");
}

void trade_party_role_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade party role: " << id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<trade_party_role_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing trade party role from database.");
}

}
