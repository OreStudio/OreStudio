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
#include "ores.iam.core/repository/account_permission_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::repository {

using namespace ores::logging;
using namespace ores::database::repository;

account_permission_repository::account_permission_repository(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<std::string>
account_permission_repository::read_effective_permissions(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading effective permissions for account: " << account_id;

    const auto account_id_str = boost::lexical_cast<std::string>(account_id);

    // Call the SQL function defined in iam_rbac_functions_create.sql
    const std::string sql =
        "SELECT code FROM ores_iam_get_effective_permissions_fn('" + account_id_str + "'::uuid)";

    return execute_raw_string_query(ctx_, sql, lg(), "Reading effective permissions for account");
}

std::vector<domain::role>
account_permission_repository::read_roles_with_permissions(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading roles with permissions for account: " << account_id;

    const auto account_id_str = boost::lexical_cast<std::string>(account_id);

    // Call the SQL function defined in iam_rbac_functions_create.sql
    const std::string sql = "SELECT role_id, role_version, role_name, role_description, "
                            "role_modified_by, permission_codes "
                            "FROM ores_iam_get_account_roles_with_permissions_fn('" +
                            account_id_str + "'::uuid)";

    const auto rows =
        execute_raw_multi_column_query(ctx_, sql, lg(), "Reading roles with permissions");

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
            result.push_back(std::move(r));
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "Read roles with permissions. Total: " << result.size();
    return result;
}

}
