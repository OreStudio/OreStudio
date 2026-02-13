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
#include "ores.iam/repository/tenant_repository.hpp"

#include <algorithm>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam/domain/tenant_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/repository/tenant_entity.hpp"
#include "ores.iam/repository/tenant_mapper.hpp"

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string tenant_repository::sql() {
    return generate_create_table_sql<tenant_entity>(lg());
}

tenant_repository::tenant_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void tenant_repository::write(const domain::tenant& tenant) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenant to database: " << tenant.name;

    execute_write_query(ctx_, tenant_mapper::map(tenant),
        lg(), "writing tenant to database");
}

void tenant_repository::write(const std::vector<domain::tenant>& tenants) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenants to database. Count: "
                               << tenants.size();

    execute_write_query(ctx_, tenant_mapper::map(tenants),
        lg(), "writing tenants to database");
}

std::vector<domain::tenant> tenant_repository::read_latest() {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tenant_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<tenant_entity, domain::tenant>(ctx_, query,
        [](const auto& entities) { return tenant_mapper::map(entities); },
        lg(), "Reading latest tenants");
}

std::vector<domain::tenant> tenant_repository::read_all_latest() {
    BOOST_LOG_SEV(lg(), debug) << "Reading all latest tenants including deleted";

    const std::string sql = "SELECT * FROM ores_iam_read_all_latest_tenants_fn()";
    const auto rows = execute_raw_multi_column_query(ctx_, sql, lg(),
        "Reading all latest tenants including deleted");

    std::vector<domain::tenant> result;
    result.reserve(rows.size());

    for (const auto& row : rows) {
        if (row.size() >= 14 && row[0] && row[1] && row[2] && row[3] && row[4] &&
            row[5] && row[7] && row[8] && row[9] && row[10] && row[11] && row[12]) {
            domain::tenant t;
            t.id = boost::lexical_cast<boost::uuids::uuid>(*row[0]);
            t.version = std::stoi(*row[2]);
            t.type = *row[3];
            t.code = *row[4];
            t.name = *row[5];
            t.description = row[6].value_or("");
            t.hostname = *row[7];
            t.status = *row[8];
            t.modified_by = *row[9];
            t.change_reason_code = *row[10];
            t.change_commentary = *row[11];
            t.recorded_at = timestamp_to_timepoint(std::string_view{*row[12]});
            result.push_back(std::move(t));
        }
    }

    // Sort by name for consistent ordering
    std::ranges::sort(result, [](const auto& a, const auto& b) {
        return a.name < b.name;
    });

    BOOST_LOG_SEV(lg(), debug) << "Read all latest tenants. Total: " << result.size();
    return result;
}

std::vector<domain::tenant>
tenant_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenant. ID: " << id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::lexical_cast<std::string>(id);
    const auto query = sqlgen::read<std::vector<tenant_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<tenant_entity, domain::tenant>(ctx_, query,
        [](const auto& entities) { return tenant_mapper::map(entities); },
        lg(), "Reading latest tenant by ID.");
}

std::vector<domain::tenant>
tenant_repository::read_latest_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenant by code: " << code;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tenant_entity>> |
        where("code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<tenant_entity, domain::tenant>(ctx_, query,
        [](const auto& entities) { return tenant_mapper::map(entities); },
        lg(), "Reading latest tenant by code.");
}

std::vector<domain::tenant>
tenant_repository::read_latest_by_hostname(const std::string& hostname) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenant by hostname: " << hostname;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<tenant_entity>> |
        where("hostname"_c == hostname && "valid_to"_c == max.value());

    return execute_read_query<tenant_entity, domain::tenant>(ctx_, query,
        [](const auto& entities) { return tenant_mapper::map(entities); },
        lg(), "Reading latest tenant by hostname.");
}

std::vector<domain::tenant>
tenant_repository::read_history(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading tenant history. ID: " << id;

    const auto id_str = boost::lexical_cast<std::string>(id);
    const auto query = sqlgen::read<std::vector<tenant_entity>> |
        where("id"_c == id_str) |
        order_by("valid_from"_c.desc());

    return execute_read_query<tenant_entity, domain::tenant>(ctx_, query,
        [](const auto& entities) { return tenant_mapper::map(entities); },
        lg(), "Reading tenant history.");
}

void tenant_repository::remove(const boost::uuids::uuid& tenant_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenant from database: " << tenant_id;

    // Delete the tenant - the database rule will close the temporal record
    // instead of actually deleting it (sets valid_to = current_timestamp)
    const auto id_str = boost::lexical_cast<std::string>(tenant_id);
    const auto query = sqlgen::delete_from<tenant_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing tenant from database");
}

}
