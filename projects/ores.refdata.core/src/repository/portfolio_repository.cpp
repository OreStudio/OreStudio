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
#include "ores.refdata.core/repository/portfolio_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata.api/domain/portfolio_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/portfolio_entity.hpp"
#include "ores.refdata.core/repository/portfolio_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string portfolio_repository::sql() {
    return generate_create_table_sql<portfolio_entity>(lg());
}

void portfolio_repository::write(context ctx, const domain::portfolio& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing portfolio: " << v.id;
    execute_write_query(ctx, portfolio_mapper::map(v),
        lg(), "Writing portfolio to database.");
}

void portfolio_repository::write(
    context ctx, const std::vector<domain::portfolio>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing portfolios. Count: " << v.size();
    execute_write_query(ctx, portfolio_mapper::map(v),
        lg(), "Writing portfolios to database.");
}

std::vector<domain::portfolio>
portfolio_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const std::string& wid = ores::utility::uuid::live_workspace_uuid_str;
    const auto query = sqlgen::read<std::vector<portfolio_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<portfolio_entity, domain::portfolio>(
        ctx, query,
        [](const auto& entities) { return portfolio_mapper::map(entities); },
        lg(), "Reading latest portfolios");
}

std::vector<domain::portfolio>
portfolio_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest portfolio. id: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const std::string& wid = ores::utility::uuid::live_workspace_uuid_str;
    const auto query = sqlgen::read<std::vector<portfolio_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<portfolio_entity, domain::portfolio>(
        ctx, query,
        [](const auto& entities) { return portfolio_mapper::map(entities); },
        lg(), "Reading latest portfolio by id.");
}

std::vector<domain::portfolio>
portfolio_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all portfolio versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const std::string& wid = ores::utility::uuid::live_workspace_uuid_str;
    const auto query = sqlgen::read<std::vector<portfolio_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<portfolio_entity, domain::portfolio>(
        ctx, query,
        [](const auto& entities) { return portfolio_mapper::map(entities); },
        lg(), "Reading all portfolio versions by id.");
}

void portfolio_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing portfolio: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::delete_from<portfolio_entity> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing portfolio from database.");
}

}
