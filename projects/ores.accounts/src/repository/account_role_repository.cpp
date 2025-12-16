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
#include "ores.accounts/repository/account_role_repository.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.accounts/repository/account_role_entity.hpp"
#include "ores.accounts/repository/account_role_mapper.hpp"

namespace ores::accounts::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::utility::log;
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
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
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

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
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

    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto role_id_str = boost::lexical_cast<std::string>(role_id);
    const auto query = sqlgen::read<std::vector<account_role_entity>> |
        where("role_id"_c == role_id_str && "valid_to"_c == max.value());

    return execute_read_query<account_role_entity, domain::account_role>(
        ctx_, query,
        [](const auto& entities) { return account_role_mapper::map(entities); },
        lg(), "Reading account-role assignments by role.");
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

    // Single query with JOINs to get all permission codes for an account
    const std::string sql =
        "SELECT DISTINCT p.code "
        "FROM oresdb.permissions p "
        "JOIN oresdb.role_permissions rp ON p.id = rp.permission_id "
        "JOIN oresdb.account_roles ar ON rp.role_id = ar.role_id "
        "WHERE ar.account_id = '" + account_id_str + "' "
        "AND p.valid_to = '9999-12-31 23:59:59' "
        "AND rp.valid_to = '9999-12-31 23:59:59' "
        "AND ar.valid_to = '9999-12-31 23:59:59' "
        "ORDER BY p.code";

    return execute_raw_string_query(ctx_, sql, lg(),
        "Reading effective permissions for account");
}

}
