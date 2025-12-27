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
#include "ores.iam/repository/permission_repository.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam/domain/permission_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam/repository/permission_entity.hpp"
#include "ores.iam/repository/permission_mapper.hpp"

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::telemetry::log;
using namespace ores::database::repository;

std::string permission_repository::sql() {
    return generate_create_table_sql<permission_entity>(lg());
}

permission_repository::permission_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void permission_repository::write(const domain::permission& permission) {
    BOOST_LOG_SEV(lg(), debug) << "Writing permission to database: "
                               << permission.code;

    execute_write_query(ctx_, permission_mapper::map(permission),
        lg(), "writing permission to database");
}

void permission_repository::write(const std::vector<domain::permission>& permissions) {
    BOOST_LOG_SEV(lg(), debug) << "Writing permissions to database. Count: "
                               << permissions.size();

    execute_write_query(ctx_, permission_mapper::map(permissions),
        lg(), "writing permissions to database");
}

std::vector<domain::permission> permission_repository::read_latest() {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<permission_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("code"_c);

    return execute_read_query<permission_entity, domain::permission>(ctx_, query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(), "Reading latest permissions");
}

std::vector<domain::permission>
permission_repository::read_latest(const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permission. ID: " << id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto id_str = boost::lexical_cast<std::string>(id);
    const auto query = sqlgen::read<std::vector<permission_entity>> |
        where("id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<permission_entity, domain::permission>(ctx_, query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(), "Reading latest permission by ID.");
}

std::vector<domain::permission>
permission_repository::read_latest_by_code(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permission by code: " << code;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<permission_entity>> |
        where("code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<permission_entity, domain::permission>(ctx_, query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(), "Reading latest permission by code.");
}

void permission_repository::remove(const boost::uuids::uuid& permission_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing permission from database: "
                               << permission_id;

    // Delete the permission - the database rule will close the temporal record
    // instead of actually deleting it (sets valid_to = current_timestamp)
    const auto id_str = boost::lexical_cast<std::string>(permission_id);
    const auto query = sqlgen::delete_from<permission_entity> |
        where("id"_c == id_str);

    execute_delete_query(ctx_, query, lg(), "removing permission from database");
}

}
