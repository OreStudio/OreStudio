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
#ifndef ORES_IAM_SERVICE_RBAC_SEEDER_HPP
#define ORES_IAM_SERVICE_RBAC_SEEDER_HPP

#include "ores.utility/log/make_logger.hpp"
#include "ores.iam/service/authorization_service.hpp"

namespace ores::iam::service {

/**
 * @brief Seeds the RBAC system with predefined permissions and roles.
 *
 * This seeder creates the standard set of permissions and roles required
 * for the application to function. It is idempotent - running it multiple
 * times will not create duplicate entries.
 *
 * Predefined Permissions:
 * - Account management: accounts:create, accounts:read, accounts:update,
 *   accounts:delete, accounts:lock, accounts:unlock
 * - Currency management: currencies:create, currencies:read, currencies:update,
 *   currencies:delete, currencies:history
 * - Feature flags: flags:create, flags:read, flags:update, flags:delete
 * - Login info: login_info:read
 * - Role management: roles:create, roles:read, roles:update, roles:delete,
 *   roles:assign, roles:revoke
 *
 * Predefined Roles:
 * - Admin: Full access (wildcard permission)
 * - Trading: currencies:read, currencies:history, flags:read
 * - Sales: currencies:read, flags:read
 * - Operations: currencies:*, flags:read, accounts:read
 * - Support: All read permissions
 */
class rbac_seeder {
private:
    inline static std::string_view logger_name =
        "ores.iam.service.rbac_seeder";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Constructs an rbac_seeder with the authorization service.
     */
    explicit rbac_seeder(authorization_service& auth_service);

    /**
     * @brief Seeds all predefined permissions and roles.
     *
     * This method is idempotent - calling it multiple times will not
     * create duplicate entries.
     *
     * @param recorded_by Username to record as the creator
     */
    void seed(const std::string& recorded_by = "system");

    /**
     * @brief Seeds only the permissions (no roles).
     *
     * @param recorded_by Username to record as the creator
     */
    void seed_permissions();

    /**
     * @brief Seeds only the roles (assumes permissions already exist).
     *
     * @param recorded_by Username to record as the creator
     */
    void seed_roles(const std::string& recorded_by);

private:
    authorization_service& auth_service_;
};

}

#endif
