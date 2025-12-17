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
#include "ores.accounts/service/rbac_seeder.hpp"

#include <stdexcept>
#include "ores.accounts/domain/permission.hpp"

namespace ores::accounts::service {

using namespace ores::utility::log;
namespace perms = domain::permissions;
namespace roles = domain::roles;

rbac_seeder::rbac_seeder(authorization_service& auth_service)
    : auth_service_(auth_service) {}

void rbac_seeder::seed(const std::string& recorded_by) {
    BOOST_LOG_SEV(lg(), info) << "Seeding RBAC permissions and roles...";
    seed_permissions();
    seed_roles(recorded_by);
    BOOST_LOG_SEV(lg(), info) << "RBAC seeding complete.";
}

void rbac_seeder::seed_permissions() {
    BOOST_LOG_SEV(lg(), info) << "Seeding permissions...";

    // Define all permissions with descriptions
    struct permission_def {
        const char* code;
        const char* description;
    };

    const std::vector<permission_def> permission_defs = {
        // Account management
        {perms::accounts_create, "Create new user accounts"},
        {perms::accounts_read, "View user account details"},
        {perms::accounts_update, "Modify user account settings"},
        {perms::accounts_delete, "Delete user accounts"},
        {perms::accounts_lock, "Lock user accounts"},
        {perms::accounts_unlock, "Unlock user accounts"},
        {perms::accounts_reset_password, "Force password reset on user accounts"},

        // Currency management
        {perms::currencies_create, "Create new currencies"},
        {perms::currencies_read, "View currency details"},
        {perms::currencies_update, "Modify currency settings"},
        {perms::currencies_delete, "Delete currencies"},
        {perms::currencies_history, "View currency version history"},

        // Feature flags
        {perms::flags_create, "Create new feature flags"},
        {perms::flags_read, "View feature flag status"},
        {perms::flags_update, "Modify feature flag settings"},
        {perms::flags_delete, "Delete feature flags"},

        // Login info
        {perms::login_info_read, "View login history and info"},

        // Role management
        {perms::roles_create, "Create new roles"},
        {perms::roles_read, "View role details"},
        {perms::roles_update, "Modify role permissions"},
        {perms::roles_delete, "Delete roles"},
        {perms::roles_assign, "Assign roles to accounts"},
        {perms::roles_revoke, "Revoke roles from accounts"},

        // Wildcard
        {perms::all, "Full access to all operations"},
    };

    std::size_t created = 0;
    std::size_t skipped = 0;

    for (const auto& [code, description] : permission_defs) {
        try {
            auth_service_.create_permission(code, description);
            ++created;
            BOOST_LOG_SEV(lg(), debug) << "Created permission: " << code;
        } catch (const std::runtime_error& e) {
            // Permission already exists
            ++skipped;
            BOOST_LOG_SEV(lg(), debug) << "Permission already exists: " << code;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Permissions seeded: " << created
                              << " created, " << skipped << " skipped.";
}

void rbac_seeder::seed_roles(const std::string& recorded_by) {
    BOOST_LOG_SEV(lg(), info) << "Seeding roles...";

    // Define roles and their permissions
    struct role_def {
        const char* name;
        const char* description;
        std::vector<const char*> permissions;
    };

    const std::vector<role_def> role_defs = {
        {
            roles::admin,
            "Full administrative access to all system functions",
            {perms::all}
        },
        {
            roles::trading,
            "Trading operations - currency read access",
            {perms::currencies_read, perms::currencies_history, perms::flags_read}
        },
        {
            roles::sales,
            "Sales operations - read-only currency access",
            {perms::currencies_read, perms::flags_read}
        },
        {
            roles::operations,
            "Operations - currency management and account viewing",
            {
                perms::currencies_create, perms::currencies_read,
                perms::currencies_update, perms::currencies_delete,
                perms::currencies_history, perms::flags_read,
                perms::accounts_read
            }
        },
        {
            roles::support,
            "Support - read-only access to all resources and admin screens",
            {
                perms::accounts_read, perms::currencies_read,
                perms::currencies_history, perms::flags_read,
                perms::login_info_read, perms::roles_read
            }
        },
    };

    std::size_t created = 0;
    std::size_t skipped = 0;

    for (const auto& [name, description, permissions] : role_defs) {
        try {
            std::vector<std::string> perm_codes;
            perm_codes.reserve(permissions.size());
            for (const auto* p : permissions) {
                perm_codes.emplace_back(p);
            }

            auth_service_.create_role(name, description, perm_codes, recorded_by);
            ++created;
            BOOST_LOG_SEV(lg(), debug) << "Created role: " << name;
        } catch (const std::runtime_error& e) {
            // Role already exists
            ++skipped;
            BOOST_LOG_SEV(lg(), debug) << "Role already exists: " << name;
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Roles seeded: " << created
                              << " created, " << skipped << " skipped.";
}

}
