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
#include "ores.refdata.core/repository/fra_convention_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.refdata.api/domain/fra_convention_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/fra_convention_entity.hpp"
#include "ores.refdata.core/repository/fra_convention_mapper.hpp"

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string fra_convention_repository::sql() {
    return generate_create_table_sql<fra_convention_entity>(lg());
}

void fra_convention_repository::write(context ctx, const domain::fra_convention& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing FRA convention: " << v.id;
    execute_write_query(ctx, fra_convention_mapper::map(v),
        lg(), "Writing FRA convention to database.");
}

void fra_convention_repository::write(
    context ctx, const std::vector<domain::fra_convention>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing FRA conventions. Count: " << v.size();
    execute_write_query(ctx, fra_convention_mapper::map(v),
        lg(), "Writing FRA conventions to database.");
}

std::vector<domain::fra_convention>
fra_convention_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<fra_convention_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<fra_convention_entity, domain::fra_convention>(
        ctx, query,
        [](const auto& entities) { return fra_convention_mapper::map(entities); },
        lg(), "Reading latest FRA conventions");
}

std::vector<domain::fra_convention>
fra_convention_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest FRA convention. id: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<fra_convention_entity>> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<fra_convention_entity, domain::fra_convention>(
        ctx, query,
        [](const auto& entities) { return fra_convention_mapper::map(entities); },
        lg(), "Reading latest FRA convention by id.");
}

std::vector<domain::fra_convention>
fra_convention_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all FRA convention versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<fra_convention_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<fra_convention_entity, domain::fra_convention>(
        ctx, query,
        [](const auto& entities) { return fra_convention_mapper::map(entities); },
        lg(), "Reading all FRA convention versions by id.");
}

void fra_convention_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing FRA convention: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<fra_convention_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing FRA convention from database.");
}

}
