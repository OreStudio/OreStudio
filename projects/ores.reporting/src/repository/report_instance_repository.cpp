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
#include "ores.reporting/repository/report_instance_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.reporting/domain/report_instance_json_io.hpp" // IWYU pragma: keep.
#include "ores.reporting/repository/report_instance_entity.hpp"
#include "ores.reporting/repository/report_instance_mapper.hpp"

namespace ores::reporting::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string report_instance_repository::sql() {
    return generate_create_table_sql<report_instance_entity>(lg());
}

void report_instance_repository::write(context ctx, const domain::report_instance& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report instance: " << v.id;
    execute_write_query(ctx, report_instance_mapper::map(v),
        lg(), "Writing report instance to database.");
}

void report_instance_repository::write(
    context ctx, const std::vector<domain::report_instance>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing report instances. Count: " << v.size();
    execute_write_query(ctx, report_instance_mapper::map(v),
        lg(), "Writing report instances to database.");
}

std::vector<domain::report_instance>
report_instance_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<report_instance_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<report_instance_entity, domain::report_instance>(
        ctx, query,
        [](const auto& entities) { return report_instance_mapper::map(entities); },
        lg(), "Reading latest report instances");
}

std::vector<domain::report_instance>
report_instance_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest report instance. id: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<report_instance_entity>> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<report_instance_entity, domain::report_instance>(
        ctx, query,
        [](const auto& entities) { return report_instance_mapper::map(entities); },
        lg(), "Reading latest report instance by id.");
}

std::vector<domain::report_instance>
report_instance_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all report instance versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<report_instance_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<report_instance_entity, domain::report_instance>(
        ctx, query,
        [](const auto& entities) { return report_instance_mapper::map(entities); },
        lg(), "Reading all report instance versions by id.");
}

void report_instance_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing report instance: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<report_instance_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing report instance from database.");
}

}
