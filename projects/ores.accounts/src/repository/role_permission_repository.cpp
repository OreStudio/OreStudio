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
#include "ores.accounts/repository/role_permission_repository.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.accounts/repository/role_permission_entity.hpp"
#include "ores.accounts/repository/role_permission_mapper.hpp"

namespace ores::accounts::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::utility::log;
using namespace ores::database::repository;

std::string role_permission_repository::sql() {
    return generate_create_table_sql<role_permission_entity>(lg());
}

role_permission_repository::role_permission_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void role_permission_repository::write(
    const domain::role_permission& role_permission) {
    BOOST_LOG_SEV(lg(), debug) << "Writing role-permission assignment to database.";

    execute_write_query(ctx_, role_permission_mapper::map(role_permission),
        lg(), "writing role-permission assignment to database");
}

void role_permission_repository::write(
    const std::vector<domain::role_permission>& role_permissions) {
    BOOST_LOG_SEV(lg(), debug) << "Writing role-permission assignments to database. "
                               << "Count: " << role_permissions.size();

    execute_write_query(ctx_, role_permission_mapper::map(role_permissions),
        lg(), "writing role-permission assignments to database");
}

std::vector<domain::role_permission> role_permission_repository::read_latest() {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<role_permission_entity>> |
        where("valid_to"_c == max.value());

    return execute_read_query<role_permission_entity, domain::role_permission>(
        ctx_, query,
        [](const auto& entities) { return role_permission_mapper::map(entities); },
        lg(), "Reading latest role-permission assignments");
}

std::vector<domain::role_permission>
role_permission_repository::read_latest_by_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading permissions for role: " << role_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto role_id_str = boost::lexical_cast<std::string>(role_id);
    const auto query = sqlgen::read<std::vector<role_permission_entity>> |
        where("role_id"_c == role_id_str && "valid_to"_c == max.value());

    return execute_read_query<role_permission_entity, domain::role_permission>(
        ctx_, query,
        [](const auto& entities) { return role_permission_mapper::map(entities); },
        lg(), "Reading role-permission assignments by role.");
}

std::vector<domain::role_permission>
role_permission_repository::read_latest_by_permission(
    const boost::uuids::uuid& permission_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading roles for permission: " << permission_id;

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto permission_id_str = boost::lexical_cast<std::string>(permission_id);
    const auto query = sqlgen::read<std::vector<role_permission_entity>> |
        where("permission_id"_c == permission_id_str && "valid_to"_c == max.value());

    return execute_read_query<role_permission_entity, domain::role_permission>(
        ctx_, query,
        [](const auto& entities) { return role_permission_mapper::map(entities); },
        lg(), "Reading role-permission assignments by permission.");
}

void role_permission_repository::remove(
    const boost::uuids::uuid& role_id,
    const boost::uuids::uuid& permission_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing role-permission assignment. Role: "
                               << role_id << ", Permission: " << permission_id;

    // Delete the assignment - the database rule will close the temporal record
    // instead of actually deleting it (sets valid_to = current_timestamp)
    const auto role_id_str = boost::lexical_cast<std::string>(role_id);
    const auto permission_id_str = boost::lexical_cast<std::string>(permission_id);
    const auto query = sqlgen::delete_from<role_permission_entity> |
        where("role_id"_c == role_id_str &&
              "permission_id"_c == permission_id_str);

    execute_delete_query(ctx_, query, lg(),
        "removing role-permission assignment from database");
}

void role_permission_repository::remove_all_for_role(
    const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all permissions for role: " << role_id;

    // Delete the assignments - the database rule will close the temporal records
    // instead of actually deleting them (sets valid_to = current_timestamp)
    const auto role_id_str = boost::lexical_cast<std::string>(role_id);
    const auto query = sqlgen::delete_from<role_permission_entity> |
        where("role_id"_c == role_id_str);

    execute_delete_query(ctx_, query, lg(),
        "removing all role-permission assignments for role");
}

}
