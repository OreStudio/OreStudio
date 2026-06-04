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
#include "ores.refdata.core/repository/cds_convention_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/cds_convention_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/cds_convention_entity.hpp"
#include "ores.refdata.core/repository/cds_convention_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string cds_convention_repository::sql() {
    return generate_create_table_sql<cds_convention_entity>(lg());
}

void cds_convention_repository::write(context ctx, const domain::cds_convention& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing CDS convention: " << v.id;
    execute_write_query(
        ctx, cds_convention_mapper::map(v), lg(), "Writing CDS convention to database.");
}

void cds_convention_repository::write(context ctx, const std::vector<domain::cds_convention>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing CDS conventions. Count: " << v.size();
    execute_write_query(
        ctx, cds_convention_mapper::map(v), lg(), "Writing CDS conventions to database.");
}

std::vector<domain::cds_convention> cds_convention_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<cds_convention_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<cds_convention_entity, domain::cds_convention>(
        ctx,
        query,
        [](const auto& entities) { return cds_convention_mapper::map(entities); },
        lg(),
        "Reading latest CDS conventions");
}

std::vector<domain::cds_convention> cds_convention_repository::read_latest(context ctx,
                                                                           const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest CDS convention. id: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<cds_convention_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id &&
                             "valid_to"_c == max.value());

    return execute_read_query<cds_convention_entity, domain::cds_convention>(
        ctx,
        query,
        [](const auto& entities) { return cds_convention_mapper::map(entities); },
        lg(),
        "Reading latest CDS convention by id.");
}

std::vector<domain::cds_convention> cds_convention_repository::read_all(context ctx,
                                                                        const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all CDS convention versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<cds_convention_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id) |
                       order_by("version"_c.desc());

    return execute_read_query<cds_convention_entity, domain::cds_convention>(
        ctx,
        query,
        [](const auto& entities) { return cds_convention_mapper::map(entities); },
        lg(),
        "Reading all CDS convention versions by id.");
}

void cds_convention_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing CDS convention: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::delete_from<cds_convention_entity> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "id"_c == id &&
                             "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing CDS convention from database.");
}

}
