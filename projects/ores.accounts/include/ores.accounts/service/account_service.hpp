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

#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.accounts/domain/account.hpp"
#include "ores.accounts/repository/account_repository.hpp"
#include "ores.accounts/repository/login_info_repository.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include "ores.utility/log/make_logger.hpp"

namespace ores::accounts::service {

/**
 * @brief Service for managing user accounts including creation, listing, and deletion.
 */
class account_service {
private:
    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(
            "ores.accounts.service.account_service");
        return instance;
    }

    static void throw_if_empty(const std::string& name, const std::string& value);

public:
    using context = ores::utility::repository::context;

    /**
     * @brief Constructs an account_service with required repositories and security components.
     *
     * @param account_repo The repository for managing account data.
     * @param login_info_repo The repository for managing login tracking data.
     */
    account_service(repository::account_repository account_repo,
                    repository::login_info_repository login_info_repo);

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

    /**
     * @brief Authenticates a user and updates login tracking information.
     *
     * This method validates the provided credentials against stored account data,
     * and if successful, updates the login_info table with the current login information.
     * It also handles failed login attempts by incrementing the failed_login_info counter
     * and may lock the account after too many consecutive failures.
     *
     * @param ctx Repository context for database operations
     * @param username The username for authentication
     * @param password The plaintext password to verify
     * @param ip_address The IP address of the login attempt
     * @return The authenticated account if credentials are valid
     * @throws std::invalid_argument If username or password is empty
     * @throws std::runtime_error If account is locked or credentials are invalid
     */
    domain::account login(context ctx, const std::string& username,
        const std::string& password, const boost::asio::ip::address& ip_address);

    /**
     * @brief Unlocks an account that has been locked due to failed login attempts.
     *
     * This method resets the account's locked status and clears the failed login counter,
     * allowing the user to attempt to login again.
     *
     * @param ctx Repository context for database operations
     * @param account_id The ID of the account to unlock
     * @throws std::invalid_argument If account does not exist
     */
    void unlock_account(context ctx, const boost::uuids::uuid& account_id);

private:
    repository::account_repository account_repo_;
    repository::login_info_repository login_info_repo_;
    utility::uuid::uuid_v7_generator uuid_generator_;
};

}

#endif
