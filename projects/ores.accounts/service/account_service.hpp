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

#ifndef ORES_ACCOUNTS_SERVICE_ACCOUNT_SERVICE_HPP
#define ORES_ACCOUNTS_SERVICE_ACCOUNT_SERVICE_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <string>
#    include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include "ores.accounts/domain/account.hpp"
#include "ores.accounts/repository/account_repository.hpp"
#include "ores.accounts/repository/logins_repository.hpp"

namespace ores::accounts::service {

/**
 * @brief Service for managing user accounts including creation, listing, and deletion.
 */
class account_service {
public:
    using context = ores::utility::repository::context;

    /**
     * @brief Constructs an account_service with required repositories and security components.
     *
     * @param account_repo The repository for managing account data.
     * @param logins_repo The repository for managing login tracking data.
     */
    account_service(repository::account_repository account_repo,
                    repository::logins_repository logins_repo);

    /**
     * @brief Creates a new account with the provided details.
     *
     * This method receives non-computed fields of the account entity such as
     * email, password, username, etc. It uses the password manager to compute
     * the salt and hash, uses the account repository to create the account,
     * and adds a new login tracking entry.
     *
     * @param ctx Repository context for database operations
     * @param username The unique username for the account
     * @param email The email address for the account
     * @param password The plaintext password (will be hashed)
     * @param is_admin Whether the account should have administrative privileges
     * @return The created account with computed fields
     */
    domain::account create_account(context ctx, const std::string& username,
        const std::string& email, const std::string& password,
        const std::string& modified_by, bool is_admin = false);

    /**
     * @brief Lists all accounts in the system.
     *
     * @param ctx Repository context for database operations
     * @return Vector of all accounts
     */
    std::vector<domain::account> list_accounts(context ctx);

    /**
     * @brief Deletes an account by its ID.
     *
     * @param ctx Repository context for database operations
     * @param account_id The ID of the account to delete
     */
    void delete_account(context ctx, const boost::uuids::uuid& account_id);

private:
    repository::account_repository account_repo_;
    repository::logins_repository logins_repo_;
    utility::uuid::uuid_v7_generator uuid_generator_;
};

}

#endif
