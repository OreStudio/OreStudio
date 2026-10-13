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
#ifndef ORES_IAM_REPOSITORY_ACCOUNT_PERMISSION_REPOSITORY_HPP
#define ORES_IAM_REPOSITORY_ACCOUNT_PERMISSION_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/role.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.logging/make_logger.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <vector>

namespace ores::iam::repository {

/**
 * @brief Reads what an account may do, through the roles it holds.
 *
 * The role assignments themselves are the generated account_role_repository.
 * What an account may do joins those assignments to the roles and their
 * permissions, which is a read in the database and not a table, so it is
 * written here by hand.
 */
class ORES_IAM_CORE_EXPORT account_permission_repository {
private:
    inline static std::string_view logger_name =
        "ores.iam.repository.account_permission_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit account_permission_repository(context ctx);

    /**
     * @brief Gets all effective permission codes for an account in a single query.
     *
     * Uses joins to fetch the distinct permission codes assigned to an account
     * through its roles, avoiding the N+1 query problem.
     */
    std::vector<std::string> read_effective_permissions(const boost::uuids::uuid& account_id);

    /**
     * @brief Gets all roles assigned to an account with their permissions.
     */
    std::vector<domain::role> read_roles_with_permissions(const boost::uuids::uuid& account_id);

private:
    context ctx_;
};

}

#endif
