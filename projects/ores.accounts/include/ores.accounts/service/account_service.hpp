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
#include <boost/asio/ip/address.hpp>
#include "ores.accounts/domain/account.hpp"
#include "ores.accounts/domain/login_info.hpp"
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
   inline static std::string_view logger_name =
        "ores.accounts.service.account_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    static void throw_if_empty(const std::string& name, const std::string& value);

public:
    using context = ores::database::context;

    /**
     * @brief Constructs an account_service with required repositories and
     * security components.
     *
     * @param account_repo The repository for managing account data.
     * @param login_info_repo The repository for managing login tracking data.
     */
    explicit account_service(database::context ctx);

    /**
     * @brief Creates a new account with the provided details.
     *
     * This method receives non-computed fields of the account entity such as
     * email, password, username, etc. It uses the password manager to compute
     * the salt and hash, uses the account repository to create the account, and
     * adds a new login tracking entry.
     *
     * @param username The unique username for the account
     * @param email The email address for the account
     * @param password The plaintext password (will be hashed)
     * @param is_admin Whether the account should have administrative privileges
     * @return The created account with computed fields
     */
    domain::account create_account(const std::string& username,
        const std::string& email, const std::string& password,
        const std::string& recorded_by, bool is_admin = false);

    /**
     * @brief Lists all accounts in the system.
     *
     * @return Vector of all accounts
     */
    std::vector<domain::account> list_accounts();

    /**
     * @brief Lists accounts with pagination support.
     *
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     * @return Vector of accounts for the requested page
     */
    std::vector<domain::account> list_accounts(std::uint32_t offset,
                                                std::uint32_t limit);

    /**
     * @brief Gets the total count of active accounts.
     *
     * @return Total number of active accounts
     */
    std::uint32_t get_total_account_count();

    /**
     * @brief Lists all login info records in the system.
     *
     * @return Vector of all login info records
     */
    std::vector<domain::login_info> list_login_info();

    /**
     * @brief Deletes an account by its ID.
     *
     * @param account_id The ID of the account to delete
     */
    void delete_account(const boost::uuids::uuid& account_id);

    /**
     * @brief Authenticates a user and updates login tracking information.
     *
     * This method validates the provided credentials against stored account
     * data, and if successful, updates the login_info table with the current
     * login information. It also handles failed login attempts by incrementing
     * the failed_login_info counter and may lock the account after too many
     * consecutive failures.
     *
     * @param username The username for authentication
     * @param password The plaintext password to verify
     * @param ip_address The IP address of the login attempt
     * @return The authenticated account if credentials are valid
     * @throws std::invalid_argument If username or password is empty
     * @throws std::runtime_error If account is locked or credentials are invalid
     */
    domain::account login(const std::string& username,
        const std::string& password, const boost::asio::ip::address& ip_address);

    /**
     * @brief Locks an account, preventing login.
     *
     * This method sets the account's locked status to true, preventing
     * the user from logging in until the account is unlocked.
     *
     * @param account_id The ID of the account to lock
     * @return true if the account was locked successfully, false otherwise
     */
    bool lock_account(const boost::uuids::uuid& account_id);

    /**
     * @brief Unlocks an account that has been locked due to failed login
     * attempts or manual locking.
     *
     * This method resets the account's locked status and clears the failed
     * login counter, allowing the user to attempt to login again.
     *
     * @param account_id The ID of the account to unlock
     * @return true if the account was unlocked successfully, false otherwise
     */
    bool unlock_account(const boost::uuids::uuid& account_id);

    /**
     * @brief Logs out a user by setting their online status to false.
     *
     * This method updates the login_info table to mark the user as offline.
     *
     * @param account_id The ID of the account to log out
     * @throws std::invalid_argument If account does not exist
     */
    void logout(const boost::uuids::uuid& account_id);

    /**
     * @brief Checks if an account has admin privileges.
     *
     * @param account_id The ID of the account to check
     * @return true if the account exists and has admin privileges, false otherwise
     */
    bool is_admin(const boost::uuids::uuid& account_id);

private:
    repository::account_repository account_repo_;
    repository::login_info_repository login_info_repo_;
    utility::uuid::uuid_v7_generator uuid_generator_;
};

}

#endif
