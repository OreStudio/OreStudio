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

#ifndef ORES_IAM_SERVICE_ACCOUNT_SERVICE_HPP
#define ORES_IAM_SERVICE_ACCOUNT_SERVICE_HPP

#include <string>
#include <boost/uuid/uuid.hpp>
#include <boost/asio/ip/address.hpp>
#include "ores.iam/domain/account.hpp"
#include "ores.iam/domain/login_info.hpp"
#include "ores.iam/repository/account_repository.hpp"
#include "ores.iam/repository/login_info_repository.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::iam::service {

/**
 * @brief Service for managing user accounts including creation, listing, and deletion.
 */
class account_service {
private:
   inline static std::string_view logger_name =
        "ores.iam.service.account_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
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
     * Note: Administrative privileges are now managed through RBAC roles.
     * Use authorization_service::assign_role() to grant roles after creation.
     *
     * @param username The unique username for the account
     * @param email The email address for the account
     * @param password The plaintext password (will be hashed)
     * @param modified_by The username of the person creating the account
     * @param change_commentary Optional commentary explaining account creation
     * @return The created account with computed fields
     */
    domain::account create_account(const std::string& username,
        const std::string& email, const std::string& password,
        const std::string& modified_by,
        const std::string& change_commentary = "Account created");

    /**
     * @brief Creates a new service account for non-human entities.
     *
     * Service accounts (service, algorithm, llm) cannot login with passwords.
     * They authenticate by creating sessions directly at startup.
     *
     * @param username The unique username for the service account
     * @param email The email address for the service account
     * @param account_type The type of service account ('service', 'algorithm', 'llm')
     * @param modified_by The username of the person creating the account
     * @param change_commentary Optional commentary explaining account creation
     * @return The created service account
     * @throws std::invalid_argument If account_type is 'user' or invalid
     */
    domain::account create_service_account(const std::string& username,
        const std::string& email, const std::string& account_type,
        const std::string& modified_by,
        const std::string& change_commentary = "Service account created");

    /**
     * @brief Gets a single account by its ID.
     *
     * @param account_id The ID of the account to retrieve
     * @return The account if found, std::nullopt otherwise
     */
    std::optional<domain::account> get_account(const boost::uuids::uuid& account_id);

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
     * @brief Updates an existing account's email address.
     *
     * Username cannot be changed. This creates a new version of the account
     * in the temporal history.
     *
     * Note: Role assignments are managed via authorization_service::assign_role()
     * and revoke_role().
     *
     * @param account_id The ID of the account to update
     * @param email The new email address
     * @param modified_by The username making the change
     * @param change_reason_code The change reason code for audit trail
     * @param change_commentary Free-text commentary explaining the change
     * @return true if the account was updated successfully, false otherwise
     * @throws std::invalid_argument If account does not exist
     */
    bool update_account(const boost::uuids::uuid& account_id,
        const std::string& email, const std::string& modified_by,
        const std::string& change_reason_code,
        const std::string& change_commentary);

    /**
     * @brief Finds an account by username.
     *
     * Returns the latest version of the account matching the given username,
     * or std::nullopt if no account is found.
     *
     * @param username The username to search for
     * @return The account if found, std::nullopt otherwise
     */
    std::optional<domain::account> find_account_by_username(
        const std::string& username);

    /**
     * @brief Retrieves all historical versions of an account by username.
     *
     * Returns all versions of the account from the temporal history,
     * ordered from newest to oldest.
     *
     * @param username The username of the account
     * @return Vector of all historical versions of the account
     */
    std::vector<domain::account> get_account_history(const std::string& username);

    /**
     * @brief Sets the password_reset_required flag on an account.
     *
     * When this flag is set, the user will be forced to change their password
     * on their next login.
     *
     * @param account_id The ID of the account to flag for password reset
     * @return true if the flag was set successfully, false otherwise
     */
    bool set_password_reset_required(const boost::uuids::uuid& account_id);

    /**
     * @brief Changes the password for an account.
     *
     * Validates password strength, hashes the new password, updates the account,
     * and clears the password_reset_required flag.
     *
     * @param account_id The ID of the account to update
     * @param new_password The new plaintext password (will be hashed)
     * @return empty string on success, error message on failure
     */
    std::string change_password(const boost::uuids::uuid& account_id,
        const std::string& new_password);

    /**
     * @brief Retrieves the login_info for a specific account.
     *
     * @param account_id The ID of the account
     * @return The login_info for the account
     * @throws std::runtime_error If login_info not found
     */
    domain::login_info get_login_info(const boost::uuids::uuid& account_id);

    /**
     * @brief Updates the email address for a user's own account.
     *
     * This is for self-service email updates. Validates email format.
     *
     * @param account_id The ID of the account to update
     * @param new_email The new email address
     * @return empty string on success, error message on failure
     */
    std::string update_my_email(const boost::uuids::uuid& account_id,
        const std::string& new_email);

private:
    repository::account_repository account_repo_;
    repository::login_info_repository login_info_repo_;
    utility::uuid::uuid_v7_generator uuid_generator_;
};

}

#endif
