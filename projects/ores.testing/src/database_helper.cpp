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
#include "ores.testing/database_helper.hpp"

#include <vector>
#include "ores.testing/test_database_manager.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::testing {

using namespace ores::logging;
using ores::testing::test_database_manager;
using ores::database::service::tenant_context;

database_helper::database_helper()
    : context_(test_database_manager::make_context()) {
    // Set tenant context for tests (use test tenant if provisioned)
    set_tenant_context();
}

void database_helper::set_tenant_context() {
    auto tenant_id = test_database_manager::get_test_tenant_id_env();
    if (tenant_id.empty()) {
        tenant_id = test_database_manager::system_tenant_id;
        BOOST_LOG_SEV(lg(), debug) << "No test tenant found, using system tenant";
    } else {
        BOOST_LOG_SEV(lg(), info) << "Using test tenant: " << tenant_id;
    }

    context_ = tenant_context::with_tenant(context_, tenant_id);
    BOOST_LOG_SEV(lg(), info) << "Successfully set tenant context";
}

void database_helper::seed_rbac() {
    BOOST_LOG_SEV(lg(), info) << "Seeding minimal RBAC data for tests";

    // Execute each statement separately (libpq doesn't handle multiple statements well)
    const std::vector<std::string> statements = {
        // Create wildcard permission if not exists
        R"SQL(
        INSERT INTO ores_iam_permissions_tbl (id, code, description, valid_from, valid_to)
        SELECT gen_random_uuid(), '*', 'Wildcard permission - grants all access',
               current_timestamp, '9999-12-31 23:59:59'::timestamptz
        WHERE NOT EXISTS (
            SELECT 1 FROM ores_iam_permissions_tbl
            WHERE code = '*' AND valid_to = '9999-12-31 23:59:59'::timestamptz
        ))SQL",

        // Create Admin role if not exists
        R"SQL(
        INSERT INTO ores_iam_roles_tbl (id, version, name, description, modified_by,
            change_reason_code, change_commentary, valid_from, valid_to)
        SELECT gen_random_uuid(), 1, 'Admin', 'Full administrative access', 'test',
               'system.seed', 'Test RBAC seed data',
               current_timestamp, '9999-12-31 23:59:59'::timestamptz
        WHERE NOT EXISTS (
            SELECT 1 FROM ores_iam_roles_tbl
            WHERE name = 'Admin' AND valid_to = '9999-12-31 23:59:59'::timestamptz
        ))SQL",

        // Create Viewer role if not exists (default role for new accounts)
        R"SQL(
        INSERT INTO ores_iam_roles_tbl (id, version, name, description, modified_by,
            change_reason_code, change_commentary, valid_from, valid_to)
        SELECT gen_random_uuid(), 1, 'Viewer', 'Default role for new accounts', 'test',
               'system.seed', 'Test RBAC seed data',
               current_timestamp, '9999-12-31 23:59:59'::timestamptz
        WHERE NOT EXISTS (
            SELECT 1 FROM ores_iam_roles_tbl
            WHERE name = 'Viewer' AND valid_to = '9999-12-31 23:59:59'::timestamptz
        ))SQL",

        // Assign wildcard permission to Admin role if not exists
        R"SQL(
        INSERT INTO ores_iam_role_permissions_tbl (role_id, permission_id, valid_from, valid_to)
        SELECT r.id, p.id, current_timestamp, '9999-12-31 23:59:59'::timestamptz
        FROM ores_iam_roles_tbl r, ores_iam_permissions_tbl p
        WHERE r.name = 'Admin' AND r.valid_to = '9999-12-31 23:59:59'::timestamptz
          AND p.code = '*' AND p.valid_to = '9999-12-31 23:59:59'::timestamptz
          AND NOT EXISTS (
              SELECT 1 FROM ores_iam_role_permissions_tbl rp
              WHERE rp.role_id = r.id AND rp.permission_id = p.id
                AND rp.valid_to = '9999-12-31 23:59:59'::timestamptz
          ))SQL"
    };

    for (const auto& sql : statements) {
        const auto execute_stmt = [&](auto&& session) {
            return session->execute(sql);
        };

        const auto r = sqlgen::session(context_.connection_pool())
            .and_then(execute_stmt);

        if (!r) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to seed RBAC data: " << r.error().what();
            return;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully seeded RBAC data";
}

std::string database_helper::db_user() {
    using ores::database::repository::execute_raw_string_query;
    auto result = execute_raw_string_query(context_,
        "SELECT current_user", lg(), "db_user");
    return result.empty() ? std::string("system") : result.front();
}

}
