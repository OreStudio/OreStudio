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
#include "ores.iam/repository/account_role_repository.hpp"

#include <sstream>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam/repository/account_role_entity.hpp"
#include "ores.iam/repository/account_role_mapper.hpp"

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string account_role_repository::sql() {
    return generate_create_table_sql<account_role_entity>(lg());
}

account_role_repository::account_role_repository(context ctx)
    : ctx_(std::move(ctx)) {}

void account_role_repository::write(const domain::account_role& account_role) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account-role assignment to database.";

    execute_write_query(ctx_, account_role_mapper::map(account_role),
        lg(), "writing account-role assignment to database");
}

void account_role_repository::write(
    const std::vector<domain::account_role>& account_roles) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account-role assignments to database. "
                               << "Count: " << account_roles.size();

    execute_write_query(ctx_, account_role_mapper::map(account_roles),
        lg(), "writing account-role assignments to database");
}

std::vector<domain::account_role> account_role_repository::read_latest() {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<account_role_entity>> |
        where("valid_to"_c == max.value());

    return execute_read_query<account_role_entity, domain::account_role>(
        ctx_, query,
        [](const auto& entities) { return account_role_mapper::map(entities); },
        lg(), "Reading latest account-role assignments");
}

std::vector<domain::account_role>
account_role_repository::read_latest_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading roles for account: " << account_id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    const auto query = sqlgen::read<std::vector<account_role_entity>> |
        where("account_id"_c == account_id_str && "valid_to"_c == max.value());

    return execute_read_query<account_role_entity, domain::account_role>(
        ctx_, query,
        [](const auto& entities) { return account_role_mapper::map(entities); },
        lg(), "Reading account-role assignments by account.");
}

std::vector<domain::account_role>
account_role_repository::read_latest_by_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading accounts for role: " << role_id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto role_id_str = boost::lexical_cast<std::string>(role_id);
    const auto query = sqlgen::read<std::vector<account_role_entity>> |
        where("role_id"_c == role_id_str && "valid_to"_c == max.value());

    return execute_read_query<account_role_entity, domain::account_role>(
        ctx_, query,
        [](const auto& entities) { return account_role_mapper::map(entities); },
        lg(), "Reading account-role assignments by role.");
}

bool account_role_repository::exists(
    const boost::uuids::uuid& account_id,
    const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Checking if role " << role_id
                               << " is assigned to account " << account_id;

    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    const auto role_id_str = boost::lexical_cast<std::string>(role_id);
    const auto query = sqlgen::read<std::vector<account_role_entity>> |
        where("account_id"_c == account_id_str &&
              "role_id"_c == role_id_str &&
              "valid_to"_c == max.value()) |
        limit(1);

    const auto result = execute_read_query<account_role_entity, domain::account_role>(
        ctx_, query,
        [](const auto& entities) { return account_role_mapper::map(entities); },
        lg(), "Checking account-role assignment existence.");

    return !result.empty();
}

void account_role_repository::remove(
    const boost::uuids::uuid& account_id,
    const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account-role assignment. Account: "
                               << account_id << ", Role: " << role_id;

    // Delete the assignment - the database rule will close the temporal record
    // instead of actually deleting it (sets valid_to = current_timestamp)
    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    const auto role_id_str = boost::lexical_cast<std::string>(role_id);
    const auto query = sqlgen::delete_from<account_role_entity> |
        where("account_id"_c == account_id_str && "role_id"_c == role_id_str);

    execute_delete_query(ctx_, query, lg(),
        "removing account-role assignment from database");
}

void account_role_repository::remove_all_for_account(
    const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all roles for account: " << account_id;

    // Delete the assignments - the database rule will close the temporal records
    // instead of actually deleting them (sets valid_to = current_timestamp)
    const auto account_id_str = boost::lexical_cast<std::string>(account_id);
    const auto query = sqlgen::delete_from<account_role_entity> |
        where("account_id"_c == account_id_str);

    execute_delete_query(ctx_, query, lg(),
        "removing all account-role assignments for account");
}

std::vector<std::string>
account_role_repository::read_effective_permissions(
    const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading effective permissions for account: "
                               << account_id;

    const auto account_id_str = boost::lexical_cast<std::string>(account_id);

    // Call the SQL function defined in iam_rbac_functions_create.sql
    const std::string sql =
        "SELECT code FROM ores_iam_get_effective_permissions_fn('" +
        account_id_str + "'::uuid)";

    return execute_raw_string_query(ctx_, sql, lg(),
        "Reading effective permissions for account");
}

std::vector<domain::role>
account_role_repository::read_roles_with_permissions(
    const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading roles with permissions for account: "
                               << account_id;

    const auto account_id_str = boost::lexical_cast<std::string>(account_id);

    // Call the SQL function defined in iam_rbac_functions_create.sql
    const std::string sql =
        "SELECT role_id, role_version, role_name, role_description, "
        "role_modified_by, permission_codes "
        "FROM ores_iam_get_account_roles_with_permissions_fn('" +
        account_id_str + "'::uuid)";

    const auto rows = execute_raw_multi_column_query(ctx_, sql, lg(),
        "Reading roles with permissions");

    std::vector<domain::role> result;
    result.reserve(rows.size());

    for (const auto& row : rows) {
        if (row.size() >= 6 && row[0] && row[1] && row[2] && row[3] && row[4]) {
            domain::role r;
            r.id = boost::lexical_cast<boost::uuids::uuid>(*row[0]);
            r.version = std::stoi(*row[1]);
            r.name = *row[2];
            r.description = *row[3];
            r.modified_by = *row[4];

            // Parse comma-separated permission codes
            if (row[5]) {
                const auto& codes_str = *row[5];
                if (!codes_str.empty()) {
                    std::istringstream iss(codes_str);
                    std::string code;
                    while (std::getline(iss, code, ',')) {
                        r.permission_codes.push_back(code);
                    }
                }
            }
            result.push_back(std::move(r));
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Read roles with permissions. Total: "
                               << result.size();
    return result;
}

}
