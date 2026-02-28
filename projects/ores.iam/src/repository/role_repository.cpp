/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.iam/repository/role_repository.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam/domain/role_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/repository/role_entity.hpp"
#include "ores.iam/repository/role_mapper.hpp"

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string role_repository::sql() {
    return generate_create_table_sql<role_entity>(lg());
}

role_repository::role_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void role_repository::write(const domain::role& role) {
    BOOST_LOG_SEV(lg(), debug) << "Writing role to database: " << role.name;

    execute_write_query(ctx_, role_mapper::map(role),
        lg(), "writing role to database");
}

void role_repository::write(const std::vector<domain::role>& roles) {
    BOOST_LOG_SEV(lg(), debug) << "Writing roles to database. Count: "
                               << roles.size();

    execute_write_query(ctx_, role_mapper::map(roles),
        lg(), "writing roles to database");
}

std::vector<domain::role> role_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<role_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<role_entity, domain::role>(ctx_, query,
        [](const auto& entities) { return role_mapper::map(entities); },
        lg(), "Reading latest roles");
}

std::vector<domain::role>
role_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role. ID: " << id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::lexical_cast<std::string>(id);
    const auto query = sqlgen::read<std::vector<role_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<role_entity, domain::role>(ctx_, query,
        [](const auto& entities) { return role_mapper::map(entities); },
        lg(), "Reading latest role by ID.");
}

std::vector<domain::role>
role_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest roles with offset: "
                               << offset << " and limit: " << limit;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<role_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("name"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<role_entity, domain::role>(ctx_, query,
        [](const auto& entities) { return role_mapper::map(entities); },
        lg(), "Reading latest roles with pagination.");
}

std::uint32_t role_repository::get_total_role_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active role count";

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<role_entity>(
        sqlgen::count().as<"count">()) |
        where("valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active role count: " << count;
    return count;
}

std::vector<domain::role>
role_repository::read_latest_by_name(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest role by name: " << name;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<role_entity>> |
        where("name"_c == name && "valid_to"_c == max.value());

    return execute_read_query<role_entity, domain::role>(ctx_, query,
        [](const auto& entities) { return role_mapper::map(entities); },
        lg(), "Reading latest role by name.");
}

std::vector<domain::role>
role_repository::read_latest_by_ids(const std::vector<boost::uuids::uuid>& ids) {
    BOOST_LOG_SEV(lg(), debug) << "Reading roles by IDs. Count: " << ids.size();

    if (ids.empty()) {
        return {};
    }

    // Build array literal for PostgreSQL
    std::string array_literal = "ARRAY[";
    for (size_t i = 0; i < ids.size(); ++i) {
        if (i > 0) array_literal += ", ";
        array_literal += "'" + boost::lexical_cast<std::string>(ids[i]) + "'::uuid";
    }
    array_literal += "]";

    // Call the SQL function defined in iam_rbac_functions_create.sql
    const std::string sql =
        "SELECT id, version, name, description, modified_by "
        "FROM ores_iam_get_roles_by_ids_fn(" + array_literal + ")";

    const auto rows = execute_raw_multi_column_query(ctx_, sql, lg(),
        "Reading roles by IDs");

    std::vector<domain::role> result;
    result.reserve(rows.size());

    for (const auto& row : rows) {
        if (row.size() >= 5 && row[0] && row[1] && row[2] && row[3] && row[4]) {
            domain::role r;
            r.id = boost::lexical_cast<boost::uuids::uuid>(*row[0]);
            r.version = std::stoi(*row[1]);
            r.name = *row[2];
            r.description = *row[3];
            r.modified_by = *row[4];
            result.push_back(std::move(r));
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Read roles by IDs. Total: " << result.size();
    return result;
}

void role_repository::remove(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing role from database: " << role_id;

    // Delete the role - the database rule will close the temporal record
    // instead of actually deleting it (sets valid_to = current_timestamp)
    const auto id_str = boost::lexical_cast<std::string>(role_id);
    const auto query = sqlgen::delete_from<role_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing role from database");
}

}
