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
#include "ores.iam/service/account_service.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.iam/domain/change_reason_constants.hpp"
#include "ores.iam/security/password_manager.hpp"
#include "ores.iam/security/password_policy_validator.hpp"
#include "ores.iam/security/email_validator.hpp"

namespace ores::iam::service {

using namespace ores::logging;
namespace reason = domain::change_reason_constants;

void account_service::
throw_if_empty(const std::string& name, const std::string& value)
{
    BOOST_LOG_SEV(lg(), debug) << name << ": '" << value << "'";
    if (value.empty()) {
        BOOST_LOG_SEV(lg(), error) << name  << " cannot be empty.";
        throw std::invalid_argument(name + " cannot be empty.");
    }
}

account_service::account_service(database::context ctx)
    : account_repo_(ctx),
      login_info_repo_(ctx) {

    BOOST_LOG_SEV(lg(), debug) << "DML for account: " << account_repo_.sql();
    BOOST_LOG_SEV(lg(), debug) << "DML for login_info: " << login_info_repo_.sql();
}

domain::account
account_service::create_account(const std::string& username,
    const std::string& email, const std::string& password,
    const std::string& recorded_by,
    const std::string& change_commentary) {

    throw_if_empty("Username", username);
    throw_if_empty("Email", email);
    throw_if_empty("Password", password); // FIXME: do not log

    // Generate a new UUID for the account
    boost::uuids::random_generator gen;
    auto id = uuid_generator_();
    BOOST_LOG_SEV(lg(), debug) << "ID for new account: " << id;

    // Hash the password using the password manager
    using security::password_manager;
    auto password_hash = password_manager::create_password_hash(password);

    // Create the account object with computed fields
    // Note: Administrative privileges are now managed through RBAC roles.
    domain::account new_account {
        .version = 0, // will be set by repository
        .id = id,
        .recorded_by = recorded_by,
        .change_reason_code = std::string{reason::codes::new_record},
        .change_commentary = change_commentary,
        .username = username,
        .password_hash = password_hash,
        .password_salt = "", // FIXME remove
        .totp_secret = "",
        .email = email
    };

    std::vector<domain::account> accounts{new_account};
    account_repo_.write(accounts);

    // Create a corresponding login tracking entry
    domain::login_info li {
        .last_login = {},
        .account_id = id,
        .failed_logins = 0,
        .locked = false,
        .online = false,
        .last_ip = {},
        .last_attempt_ip = {}
    };

    std::vector<domain::login_info> login_infos{li};
    login_info_repo_.write(login_infos);

    return new_account;
}

std::optional<domain::account>
account_service::get_account(const boost::uuids::uuid& account_id) {
    auto accounts = account_repo_.read_latest(account_id);
    if (accounts.empty()) {
        return std::nullopt;
    }
    return accounts[0];
}

std::vector<domain::account> account_service::list_accounts() {
    return account_repo_.read_latest();
}

std::vector<domain::account> account_service::list_accounts(
    std::uint32_t offset, std::uint32_t limit) {
    return account_repo_.read_latest(offset, limit);
}

std::uint32_t account_service::get_total_account_count() {
    return account_repo_.get_total_account_count();
}

std::vector<domain::login_info> account_service::list_login_info() {
    return login_info_repo_.read();
}

void account_service::delete_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Deleting account: "
                               << boost::uuids::to_string(account_id);

    // Verify account exists before attempting deletion
    auto accounts = account_repo_.read_latest(account_id);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to delete non-existent account: "
                                  << boost::uuids::to_string(account_id);
        throw std::invalid_argument("Account does not exist");
    }

    // Perform bitemporal soft delete (sets valid_to = current_timestamp)
    account_repo_.remove(account_id);

    BOOST_LOG_SEV(lg(), info) << "Successfully deleted account: "
                              << boost::uuids::to_string(account_id);
}

domain::account account_service::login(const std::string& username,
    const std::string& password, const boost::asio::ip::address& ip_address) {

    throw_if_empty("Username", username);
    throw_if_empty("Password", password); // FIXME: do not log

    BOOST_LOG_SEV(lg(), debug) << "Login attempt for username: " << username
                               << " from IP: " << ip_address;

    // Read the account by username
    auto accounts = account_repo_.read_latest_by_username(username);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Login failed: account not found for username: "
                                  << username;
        throw std::runtime_error("Invalid username or password");
    }

    const auto& account = accounts[0];

    auto login_info_vec =
        login_info_repo_.read(account.id);
    if (login_info_vec.empty()) {
        BOOST_LOG_SEV(lg(), error) << "Login tracking not found for account: "
                                   << boost::uuids::to_string(account.id);
        throw std::runtime_error("Login tracking information missing");
    }

    auto login_info = login_info_vec[0];

    if (login_info.locked) {
        BOOST_LOG_SEV(lg(), warn) << "Login attempt for locked account: "
                                  << username;
        throw std::runtime_error("Account is locked due to too many failed attempts");
    }

    using security::password_manager;
    bool password_valid = password_manager::
        verify_password_hash(password, account.password_hash);

    login_info.last_attempt_ip = ip_address;

    if (!password_valid) {
        login_info.failed_logins++;
        BOOST_LOG_SEV(lg(), warn) << "Failed login attempt for username: "
                                << username << ". Attempt: "
                                << login_info.failed_logins;

        login_info_repo_.update(login_info);

        constexpr int max_failed_attempts = 5;
        if (login_info.failed_logins >= max_failed_attempts) {
            lock_account(account.id);
            BOOST_LOG_SEV(lg(), warn) << "Account locked due to too many failed attempts: "
                                      << username;
        }

        throw std::runtime_error("Invalid username or password");
    }

    login_info.last_ip = ip_address;
    login_info.last_login = std::chrono::system_clock::now();
    login_info.failed_logins = 0;
    login_info.online = true;

    BOOST_LOG_SEV(lg(), info) << "Successful login for username: " << username
                              << " from IP: " << ip_address;

    login_info_repo_.update(login_info);

    return account;
}

bool account_service::lock_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Locking account: "
                               << boost::uuids::to_string(account_id);

    auto accounts = account_repo_.read_latest(account_id);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to lock non-existent account: "
                                  << boost::uuids::to_string(account_id);
        return false;
    }

    auto login_info_vec = login_info_repo_.read(account_id);
    if (login_info_vec.empty()) {
        BOOST_LOG_SEV(lg(), error) << "Login tracking not found for account: "
                                   << boost::uuids::to_string(account_id);
        return false;
    }

    auto login_info = login_info_vec[0];

    if (login_info.locked == true) {
        BOOST_LOG_SEV(lg(), warn) << "Account is already locked.";
        return true;
    }

    login_info.locked = true;

    BOOST_LOG_SEV(lg(), info) << "Account locked: "
                              << boost::uuids::to_string(account_id);

    login_info_repo_.update(login_info);
    return true;
}

bool account_service::unlock_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Unlocking account: "
                               << boost::uuids::to_string(account_id);

    auto accounts = account_repo_.read_latest(account_id);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to unlock non-existent account: "
                                  << boost::uuids::to_string(account_id);
        return false;
    }

    auto login_info_vec = login_info_repo_.read(account_id);
    if (login_info_vec.empty()) {
        BOOST_LOG_SEV(lg(), error) << "Login tracking not found for account: "
                                   << boost::uuids::to_string(account_id);
        return false;
    }

    auto login_info = login_info_vec[0];

    if (login_info.locked == false) {
        BOOST_LOG_SEV(lg(), warn) << "Account is not locked.";
        return true;
    }

    login_info.locked = false;
    login_info.failed_logins = 0;

    BOOST_LOG_SEV(lg(), info) << "Account unlocked: "
                              << boost::uuids::to_string(account_id);

    login_info_repo_.update(login_info);
    return true;
}

void account_service::logout(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Logging out account: "
                               << boost::uuids::to_string(account_id);

    auto accounts = account_repo_.read_latest(account_id);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to logout non-existent account: "
                                  << boost::uuids::to_string(account_id);
        throw std::invalid_argument("Account does not exist");
    }

    auto login_info_vec = login_info_repo_.read(account_id);
    if (login_info_vec.empty()) {
        BOOST_LOG_SEV(lg(), error) << "Login tracking not found for account: "
                                   << boost::uuids::to_string(account_id);
        throw std::runtime_error("Login tracking information missing");
    }

    auto login_info = login_info_vec[0];
    login_info.online = false;

    BOOST_LOG_SEV(lg(), info) << "Account logged out: "
                              << boost::uuids::to_string(account_id);

    login_info_repo_.update(login_info);
}

bool account_service::update_account(const boost::uuids::uuid& account_id,
    const std::string& email, const std::string& recorded_by,
    const std::string& change_reason_code,
    const std::string& change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Updating account: "
                               << boost::uuids::to_string(account_id);

    // Verify account exists
    auto accounts = account_repo_.read_latest(account_id);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to update non-existent account: "
                                  << boost::uuids::to_string(account_id);
        throw std::invalid_argument("Account does not exist");
    }

    // Get existing account and create new version with updated fields
    auto account = accounts[0];
    account.email = email;
    account.recorded_by = recorded_by;
    account.change_reason_code = change_reason_code;
    account.change_commentary = change_commentary;
    // Note: version is NOT incremented here - the database trigger handles it
    // The trigger uses optimistic locking: new.version must match current_version

    // Write the updated account (creates new temporal version)
    account_repo_.write(account);

    BOOST_LOG_SEV(lg(), info) << "Successfully updated account: "
                              << boost::uuids::to_string(account_id)
                              << ", new version: " << account.version;

    return true;
}

std::vector<domain::account>
account_service::get_account_history(const std::string& username) {
    BOOST_LOG_SEV(lg(), debug) << "Getting account history for username: "
                               << username;

    // First look up the account by username to get the ID
    auto accounts = account_repo_.read_latest_by_username(username);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Account not found for username: "
                                  << username;
        return {};
    }

    const auto& account_id = accounts[0].id;
    BOOST_LOG_SEV(lg(), debug) << "Found account ID: "
                               << boost::uuids::to_string(account_id);

    // Get all versions of the account by ID
    auto all_versions = account_repo_.read_all(account_id);
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << all_versions.size()
                              << " historical versions for account: " << username;

    return all_versions;
}

bool account_service::set_password_reset_required(
    const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Setting password_reset_required for account: "
                               << boost::uuids::to_string(account_id);

    auto login_info_vec = login_info_repo_.read(account_id);
    if (login_info_vec.empty()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Account or login tracking not found for account: "
            << boost::uuids::to_string(account_id);
        return false;
    }

    auto login_info = login_info_vec[0];

    if (login_info.password_reset_required) {
        BOOST_LOG_SEV(lg(), debug)
            << "Password reset is already required for account.";
        return true;
    }

    login_info.password_reset_required = true;

    BOOST_LOG_SEV(lg(), info) << "Password reset required set for account: "
                              << boost::uuids::to_string(account_id);

    login_info_repo_.update(login_info);
    return true;
}

std::string account_service::change_password(const boost::uuids::uuid& account_id,
    const std::string& new_password) {
    BOOST_LOG_SEV(lg(), debug) << "Changing password for account: "
                               << boost::uuids::to_string(account_id);

    // Validate password strength using policy validator
    using security::password_policy_validator;
    auto validation = password_policy_validator::validate(new_password);
    if (!validation.is_valid) {
        return validation.error_message;
    }

    // Verify account exists
    auto accounts = account_repo_.read_latest(account_id);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to change password for non-existent account: "
                                  << boost::uuids::to_string(account_id);
        return "Account does not exist";
    }

    // Check that new password is different from current password
    using security::password_manager;
    const auto& current_hash = accounts[0].password_hash;
    if (password_manager::verify_password_hash(new_password, current_hash)) {
        BOOST_LOG_SEV(lg(), debug) << "New password matches current password";
        return "New password must be different from current password";
    }

    // Hash the new password
    auto password_hash = password_manager::create_password_hash(new_password);

    // Update account with new password hash
    auto account = accounts[0];
    account.password_hash = password_hash;
    account.change_reason_code = std::string{reason::codes::non_material_update};
    account.change_commentary = "Password changed";
    // Note: version is NOT incremented here - the database trigger handles it
    // The trigger uses optimistic locking: new.version must match current_version

    // Write the updated account (creates new temporal version)
    account_repo_.write(account);

    // Clear password_reset_required flag
    auto login_info_vec = login_info_repo_.read(account_id);
    if (!login_info_vec.empty()) {
        auto login_info = login_info_vec[0];
        if (login_info.password_reset_required) {
            login_info.password_reset_required = false;
            login_info_repo_.update(login_info);
            BOOST_LOG_SEV(lg(), debug) << "Cleared password_reset_required flag";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully changed password for account: "
                              << boost::uuids::to_string(account_id);

    return ""; // Empty string indicates success
}

domain::login_info account_service::get_login_info(
    const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting login_info for account: "
                               << boost::uuids::to_string(account_id);

    auto login_info_vec = login_info_repo_.read(account_id);
    if (login_info_vec.empty()) {
        BOOST_LOG_SEV(lg(), error) << "Login tracking not found for account: "
                                   << boost::uuids::to_string(account_id);
        throw std::runtime_error("Login tracking information missing");
    }

    return login_info_vec[0];
}

std::string account_service::update_my_email(const boost::uuids::uuid& account_id,
    const std::string& new_email) {
    BOOST_LOG_SEV(lg(), debug) << "Updating email for account: "
                               << boost::uuids::to_string(account_id);

    // Validate email format
    using security::email_validator;
    auto validation = email_validator::validate(new_email);
    if (!validation.is_valid) {
        return validation.error_message;
    }

    // Verify account exists
    auto accounts = account_repo_.read_latest(account_id);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to update email for non-existent account: "
                                  << boost::uuids::to_string(account_id);
        return "Account does not exist";
    }

    // Check if email is the same
    if (accounts[0].email == new_email) {
        return "New email is the same as current email";
    }

    // Update account with new email
    auto account = accounts[0];
    account.email = new_email;
    account.change_reason_code = std::string{reason::codes::non_material_update};
    account.change_commentary = "Email address changed";
    // Note: version is NOT incremented here - the database trigger handles it

    // Write the updated account (creates new temporal version)
    account_repo_.write(account);

    BOOST_LOG_SEV(lg(), info) << "Successfully updated email for account: "
                              << boost::uuids::to_string(account_id);

    return ""; // Empty string indicates success
}

}
