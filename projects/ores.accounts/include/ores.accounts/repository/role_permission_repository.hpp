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
#ifndef ORES_ACCOUNTS_REPOSITORY_ROLE_PERMISSION_REPOSITORY_HPP
#define ORES_ACCOUNTS_REPOSITORY_ROLE_PERMISSION_REPOSITORY_HPP

#include <map>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.accounts/domain/role_permission.hpp"

namespace ores::accounts::repository {

/**
 * @brief Reads and writes role-permission assignments to data storage.
 */
class role_permission_repository {
private:
    inline static std::string_view logger_name =
        "ores.accounts.repository.role_permission_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit role_permission_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes role-permission assignments to database.
     */
    /**@{*/
    void write(const domain::role_permission& role_permission);
    void write(const std::vector<domain::role_permission>& role_permissions);
    /**@}*/

    /**
     * @brief Reads all active role-permission assignments.
     */
    std::vector<domain::role_permission> read_latest();

    /**
     * @brief Reads all permissions assigned to a specific role.
     */
    std::vector<domain::role_permission>
    read_latest_by_role(const boost::uuids::uuid& role_id);

    /**
     * @brief Reads all roles that have a specific permission.
     */
    std::vector<domain::role_permission>
    read_latest_by_permission(const boost::uuids::uuid& permission_id);

    /**
     * @brief Removes a specific role-permission assignment.
     */
    void remove(const boost::uuids::uuid& role_id,
                const boost::uuids::uuid& permission_id);

    /**
     * @brief Removes all permission assignments for a role.
     */
    void remove_all_for_role(const boost::uuids::uuid& role_id);

    /**
     * @brief Gets all role-permission mappings with permission codes in a single query.
     *
     * Returns a map from role_id (as string) to vector of permission codes.
     * Uses JOINs to efficiently fetch all data in one database round-trip.
     */
    std::map<std::string, std::vector<std::string>> read_all_role_permission_codes();

private:
    context ctx_;
};

}

#endif
