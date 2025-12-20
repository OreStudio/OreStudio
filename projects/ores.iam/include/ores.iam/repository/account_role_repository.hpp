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
#ifndef ORES_ACCOUNTS_REPOSITORY_ACCOUNT_ROLE_REPOSITORY_HPP
#define ORES_ACCOUNTS_REPOSITORY_ACCOUNT_ROLE_REPOSITORY_HPP

#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.iam/domain/account_role.hpp"
#include "ores.iam/domain/role.hpp"

namespace ores::iam::repository {

/**
 * @brief Reads and writes account-role assignments to data storage.
 */
class account_role_repository {
private:
    inline static std::string_view logger_name =
        "ores.iam.repository.account_role_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit account_role_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes account-role assignments to database.
     */
    /**@{*/
    void write(const domain::account_role& account_role);
    void write(const std::vector<domain::account_role>& account_roles);
    /**@}*/

    /**
     * @brief Reads all active account-role assignments.
     */
    std::vector<domain::account_role> read_latest();

    /**
     * @brief Reads all roles assigned to a specific account.
     */
    std::vector<domain::account_role>
    read_latest_by_account(const boost::uuids::uuid& account_id);

    /**
     * @brief Reads all accounts assigned to a specific role.
     */
    std::vector<domain::account_role>
    read_latest_by_role(const boost::uuids::uuid& role_id);

    /**
     * @brief Checks if a specific account-role assignment exists.
     *
     * More efficient than read_latest_by_account when only checking existence.
     */
    bool exists(const boost::uuids::uuid& account_id,
                const boost::uuids::uuid& role_id);

    /**
     * @brief Removes a specific account-role assignment.
     */
    void remove(const boost::uuids::uuid& account_id,
                const boost::uuids::uuid& role_id);

    /**
     * @brief Removes all role assignments for an account.
     */
    void remove_all_for_account(const boost::uuids::uuid& account_id);

    /**
     * @brief Gets all effective permission codes for an account in a single query.
     *
     * This method uses JOINs to efficiently fetch all distinct permission codes
     * assigned to an account through its roles, avoiding the N+1 query problem.
     */
    std::vector<std::string>
    read_effective_permissions(const boost::uuids::uuid& account_id);

    /**
     * @brief Gets all roles assigned to an account with their permissions.
     *
     * This method efficiently fetches all roles for an account along with
     * their permission codes in a single database query, avoiding N+1 issues.
     */
    std::vector<domain::role>
    read_roles_with_permissions(const boost::uuids::uuid& account_id);

private:
    context ctx_;
};

}

#endif
