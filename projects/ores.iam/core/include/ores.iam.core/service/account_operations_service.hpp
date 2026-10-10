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

#include "ores.iam.api/domain/account.hpp"
#include "ores.iam.api/domain/account_credential.hpp"
#include "ores.iam.api/domain/login_info.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/account_contact_information_repository.hpp"
#include "ores.iam.core/repository/account_credential_repository.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.iam.core/repository/login_info_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <boost/asio/ip/address.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <stdexcept>
#include <string>

namespace ores::iam::service {

/**
 * @brief Service for managing user accounts including creation, listing, and deletion.
 */
/**
 * @brief The account a login authenticated, and what its own record says the
 * next sign-in must do.
 *
 * The reset flag lives on the account's login record rather than on the
 * account, and the login path reads that record to check it is not locked, so
 * the answer carries the flag rather than a second read of the same row.
 */
struct ORES_IAM_CORE_EXPORT authenticated_login {
    domain::account account;
    bool password_reset_required = false;
};

/**
 * @brief A login refusal that states why, as a code a client can branch on.
 *
 * The sentence is for a person and may change; the code is what a screen
 * decides with. A refusal that carries only English is why a client cannot
 * tell a locked account from a wrong password today.
 */
struct ORES_IAM_CORE_EXPORT login_error : std::runtime_error {
    login_error(ores::utility::serialization::error_code code, const std::string& message)
        : std::runtime_error(message)
        , code(code) {}

    ores::utility::serialization::error_code code;
};

class ORES_IAM_CORE_EXPORT account_operations_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.account_operations_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    static void throw_if_empty(const std::string& name, const std::string& value);

public:
    using context = ores::database::context;

    /**
     * @brief Constructs an account_operations_service with required repositories and
     * security components.
     *
     * @param account_repo The repository for managing account data.
     * @param login_info_repo The repository for managing login tracking data.
     */
    explicit account_operations_service(database::context ctx);

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
     * @param full_name The name the account is shown by, empty when it has none
     * @return The created account with computed fields
     */
    domain::account create_account(const std::string& username,
                                   const std::string& email,
                                   const std::string& password,
                                   const std::string& modified_by,
                                   const std::string& change_commentary = "Account created",
                                   const std::string& full_name = "");

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
    domain::account
    create_service_account(const std::string& username,
                           const std::string& email,
                           const std::string& account_type,
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
    std::vector<domain::account> list_accounts(std::uint32_t offset, std::uint32_t limit);

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
    authenticated_login login(const std::string& username,
                              const std::string& password,
                              const boost::asio::ip::address& ip_address);

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
     * @param full_name The account holder's full name; empty clears it.
     * @param default_party_id The party to set as the account's default
     * quick-login party; nullopt clears it. The caller is responsible for
     * verifying the party is one the account is actually associated with.
     * @param job_title Job title / functional role; empty clears it.
     * @param reports_to_account_id The account this person reports to;
     * nil clears it. The caller is responsible for verifying the target
     * account exists (the insert trigger also enforces this).
     * @param image_id Profile picture for this account; nil clears it.
     * The caller is responsible for verifying the image exists (the
     * insert trigger also enforces this).
     * @param modified_by The username making the change
     * @param change_reason_code The change reason code for audit trail
     * @param change_commentary Free-text commentary explaining the change
     * @return true if the account was updated successfully, false otherwise
     * @throws std::invalid_argument If account does not exist
     */
    bool update_account(const boost::uuids::uuid& account_id,
                        const std::string& email,
                        const std::string& full_name,
                        const std::optional<boost::uuids::uuid>& default_party_id,
                        const std::string& job_title,
                        const boost::uuids::uuid& reports_to_account_id,
                        const boost::uuids::uuid& image_id,
                        const std::string& modified_by,
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
    std::optional<domain::account> find_account_by_username(const std::string& username);

    /**
     * @brief Finds an account by id.
     *
     * Returns the latest version of the account, or std::nullopt if no
     * account is found.
     *
     * @param account_id The account id to search for
     * @return The account if found, std::nullopt otherwise
     */
    std::optional<domain::account> find_account_by_id(const boost::uuids::uuid& account_id);

    /**
     * @brief Retrieves all historical versions of an account by username.
     *
     * Returns all versions of the account from the temporal history,
     * ordered from newest to oldest.
     *
     * @param username The username of the account
     * @return Vector of all historical versions of the account
     */

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
     * @brief Changes an account's own password, proving the current one.
     *
     * Verifies @p current_password against the stored hash before anything is
     * written, and refuses the change when it is empty or does not match.
     * Validates password strength, hashes the new password, updates the
     * account, and clears the password_reset_required flag.
     *
     * @param account_id The ID of the account to update
     * @param current_password The caller's existing plaintext password
     * @param new_password The new plaintext password (will be hashed)
     * @return empty string on success, error message on failure
     */
    std::string change_password(const boost::uuids::uuid& account_id,
                                const std::string& current_password,
                                const std::string& new_password);

    /**
     * @brief Sets a new password without proving the current one.
     *
     * The administrator reset path, reached only through
     * iam.v1.ops.reset_password, whose handler checks
     * iam::accounts:reset_password first. Validates password strength, hashes
     * the new password, updates the account, and clears the
     * password_reset_required flag.
     *
     * @param account_id The ID of the account to update
     * @param new_password The new plaintext password (will be hashed)
     * @return empty string on success, error message on failure
     */
    std::string reset_password(const boost::uuids::uuid& account_id,
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
    std::string update_my_email(const boost::uuids::uuid& account_id, const std::string& new_email);

    /**
     * @brief Sets or clears the default party for a user's own account.
     *
     * Self-service; the caller is responsible for verifying the party is
     * one the account is actually associated with before calling this.
     *
     * @param account_id The ID of the account to update
     * @param party_id The party to set as the account's default, or nothing to
     * clear the stored default
     * @return empty string on success, error message on failure
     */
    std::string set_my_default_party(const boost::uuids::uuid& account_id,
                                     const std::optional<boost::uuids::uuid>& party_id);

    /**
     * @brief Writes who one account reports to, and nothing else.
     *
     * The write names one field, so a field changed elsewhere between the
     * caller's read and this write is not overwritten. A stated expected
     * version that no longer matches the account is refused with a conflict,
     * rather than applied to a row the caller has not seen.
     *
     * @param request The account, the manager (empty clears the line), the
     * version the caller read, and the reason
     * @return The shared result plus the account as written, when the write
     * succeeded
     */
    messaging::set_reporting_line_response
    set_reporting_line(const messaging::set_reporting_line_request& request);

    /**
     * @brief Reads a reporting shape in one call: the tenant's, or what a viewer
     * may see.
     *
     * The accounts come from the tenant's own roster, which row-level security
     * bounds. With a @p viewer the roster is cut to the people who work in any
     * party the viewer's account works in and everyone who reports to the
     * viewer, directly or indirectly. A manager outside that set is not in the
     * tree and the account carries a marker saying so, so a person who has a
     * manager is never drawn as one who has none. The parties are those of the
     * people in the answer. An unstated root answers the whole scope, ordered
     * by depth; a stated root answers that account's branch. An account that
     * reaches no root is answered with depth -1 and counted in the response.
     *
     * @param request The root to answer from, or empty for the whole scope
     * @param viewer The caller's account when the read is scoped to what it may
     * see, or nothing for the whole tenant
     * @return The shared result plus the nodes, the parties, and how many
     * accounts reach no root
     */
    messaging::get_reporting_tree_response
    get_reporting_tree(const messaging::get_reporting_tree_request& request,
                       const std::optional<boost::uuids::uuid>& viewer = std::nullopt);

    /**
     * @brief Writes the fields a member owns on their own account.
     *
     * The account is the caller's own, named by the session rather than by
     * the request. The three fields a member owns are written as the request
     * states them. A request that states any of the three fields only an
     * administrator owns is denied, and the result names each refused field
     * with the code field_not_self_writable.
     *
     * @param request The profile write
     * @param account_id The account of the caller's session
     * @return The shared result plus the account as written, when the write
     * landed
     */
    messaging::update_self_account_response
    update_self_account(const messaging::update_self_account_request& request,
                        const boost::uuids::uuid& account_id);

    /**
     * @brief Writes the contact record of the caller's own account.
     *
     * The record is found from the account id, which the session states, not
     * from anything in the request, so a caller cannot name another account's
     * record. A member who has no contact record gets one.
     *
     * @param request The contact write
     * @param account_id The account of the caller's session
     * @return The shared result plus the record as written, when the write
     * landed
     */
    messaging::update_self_account_contact_information_response
    update_self_account_contact_information(
        const messaging::update_self_account_contact_information_request& request,
        const boost::uuids::uuid& account_id);

    /**
     * @brief Reads the contact record of the caller's own account.
     *
     * The record is found from the account id, which the session states, so a
     * caller cannot name another account's record.
     *
     * @param account_id The account of the caller's session
     * @return The shared result plus the record, or no record when the account
     * has none yet
     */
    messaging::get_my_account_contact_information_response
    get_my_account_contact_information(const boost::uuids::uuid& account_id);

    /**
     * @brief Reads the caller's own account.
     *
     * The account is found from the id the session states, so a caller cannot
     * name another account. Nothing here checks a permission: the read is a
     * self read on the allow-list of Authorised reads.
     *
     * @param account_id The account of the caller's session
     * @return The shared result plus the account, or no account when the
     * session names none
     */
    messaging::get_my_account_response get_my_account(const boost::uuids::uuid& account_id);

    /**
     * @brief Reads the caller's own sign-in state.
     *
     * The account comes from the validated token and is passed in by the
     * handler, so the read cannot name another account's. It needs no
     * permission: it is a self read on the allow-list of Authorised reads.
     *
     * @param account_id The account of the caller's session
     * @return The shared result plus the sign-in state, or none when nothing
     * is recorded
     */
    messaging::get_my_login_info_response get_my_login_info(const boost::uuids::uuid& account_id);

    /**
     * @brief Reads the caller's own open sessions, newest first.
     *
     * The account comes from the validated token. It needs no permission: it
     * is a self read on the allow-list of Authorised reads.
     *
     * @param account_id The account of the caller's session
     * @return The shared result plus the sessions with no end time
     */
    messaging::get_my_sessions_response get_my_sessions(const boost::uuids::uuid& account_id);

    /**
     * @brief Authenticates a service account by the machine password it holds.
     *
     * The account is resolved by username and its credential row by the
     * account's id, so neither read crosses tenants or reaches a neighbouring
     * account. A user account never authenticates this way, and a service
     * account whose credential row is missing or carries no service hash is
     * refused with a log line that says so: a data fault must not read as a
     * rejected password.
     *
     * @param username The service account's username
     * @param password The plaintext machine password to verify
     * @return The account's id when the credentials hold, std::nullopt otherwise
     */
    std::optional<boost::uuids::uuid> verify_service_credentials(const std::string& username,
                                                                 const std::string& password);

private:
    /**
     * @brief Reads the open credential row of an account, if it has one.
     */
    std::optional<domain::account_credential> read_credential(const boost::uuids::uuid& account_id);

    /**
     * @brief Writes the credential row an account's write produced.
     *
     * A row the caller read is replaced through the version it states; an
     * account that has no credential row gets one. Either way the store
     * decides, so a write over a credential that moved on is a conflict
     * rather than a silent overwrite.
     */
    void write_credential(domain::account_credential credential);

    repository::account_repository account_repo_;
    repository::account_credential_repository credential_repo_;
    repository::account_contact_information_repository contact_repo_;
    repository::login_info_repository login_info_repo_;
    database::context ctx_;
    utility::uuid::uuid_v7_generator uuid_generator_;
};

}

#endif
