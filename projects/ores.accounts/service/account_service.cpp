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

#include "account_service.hpp"

#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.utility/log/logger.hpp"
#include "ores.accounts/security/password_manager.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.accounts.service.account_service"));

void throw_if_empty(const std::string& name, const std::string& value)
{
    BOOST_LOG_SEV(lg, debug) << name << ": '" << value << "'";
    if (value.empty()) {
        BOOST_LOG_SEV(lg, error) << name  << " cannot be empty.";
        throw std::invalid_argument(name + " cannot be empty.");
    }
}

}

namespace ores::accounts::service {

account_service::account_service(repository::account_repository account_repo,
    repository::login_info_repository login_info_repo)
    : account_repo_(std::move(account_repo)),
      login_info_repo_(std::move(login_info_repo)) {

    BOOST_LOG_SEV(lg, debug) << "DML for account: " << account_repo_.sql();
    BOOST_LOG_SEV(lg, debug) << "DML for account: " << login_info_repo_.sql();
}

domain::account account_service::
create_account(context ctx, const std::string& username, const std::string& email,
    const std::string& password, const std::string& modified_by, bool is_admin) {

    throw_if_empty("Username", username);
    throw_if_empty("Email", email);
    throw_if_empty("Password", password); // FIXME: do not log

    // Generate a new UUID for the account
    boost::uuids::random_generator gen;
    auto id = uuid_generator_();
    BOOST_LOG_SEV(lg, debug) << "ID for new account: " << id;

    // Hash the password using the password manager
    using security::password_manager;
    auto password_hash = password_manager::create_password_hash(password);

    // Create the account object with computed fields
    domain::account new_account {
        .version = 0, // will be set by repository
        .modified_by = modified_by,
        .id = id,
        .username = username,
        .password_hash = password_hash,
        .password_salt = "", // FIXME remove
        .totp_secret = "",
        .email = email,
        .is_admin = is_admin
    };

    std::vector<domain::account> accounts{new_account};
    account_repo_.write(ctx, accounts);

    // Create a corresponding login tracking entry
    domain::login_info li{
        .account_id = id,
        .last_ip = {},
        .last_attempt_ip = {},
        .failed_logins = 0,
        .locked = false,
        .last_login = {},
        .online = false
    };

    std::vector<domain::login_info> login_infos{li};
    login_info_repo_.write(ctx, login_infos);

    return new_account;
}

std::vector<domain::account> account_service::list_accounts(context ctx) {
    return account_repo_.read_latest(ctx);
}

void account_service::delete_account(context ctx, const boost::uuids::uuid& account_id) {
    // For now we'll just read the account and return - in a real implementation
    // this would remove the account from the database, but that requires
    // additional repository method support
    auto accounts = account_repo_.read_latest(ctx, account_id);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg, warn) << "Attempted to delete non-existent account: "
                                << boost::uuids::to_string(account_id);
        throw std::invalid_argument("Account does not exist");
    }

    // TODO: In a complete implementation, this would remove the account
    // and its associated login tracking information from the database
    BOOST_LOG_SEV(lg, warn) << "Account deletion not fully implemented";
}

domain::account account_service::login(context ctx, const std::string& username,
    const std::string& password, const boost::asio::ip::address& ip_address) {

    throw_if_empty("Username", username);
    throw_if_empty("Password", password); // FIXME: do not log

    BOOST_LOG_SEV(lg, debug) << "Login attempt for username: " << username
                             << " from IP: " << ip_address;

    // Read the account by username
    auto accounts = account_repo_.read_latest_by_username(ctx, username);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg, warn) << "Login failed: account not found for username: "
                                << username;
        throw std::runtime_error("Invalid username or password");
    }

    const auto& account = accounts[0];

    // Read the login tracking information
    auto login_info_vec = login_info_repo_.read(ctx, account.id);
    if (login_info_vec.empty()) {
        BOOST_LOG_SEV(lg, error) << "Login tracking not found for account: "
                                 << boost::uuids::to_string(account.id);
        throw std::runtime_error("Login tracking information missing");
    }

    auto login_info = login_info_vec[0];

    // Check if account is locked
    if (login_info.locked) {
        BOOST_LOG_SEV(lg, warn) << "Login attempt for locked account: " << username;
        throw std::runtime_error("Account is locked due to too many failed login attempts");
    }

    // Verify the password
    using security::password_manager;
    bool password_valid = password_manager::
        verify_password_hash(password, account.password_hash);

    // Update login tracking based on authentication result
    login_info.last_attempt_ip = ip_address;

    if (!password_valid) {
        // Increment failed login counter
        login_info.failed_logins++;
        BOOST_LOG_SEV(lg, warn) << "Failed login attempt for username: " << username
                                << " (attempt " << login_info.failed_logins << ")";

        // Lock account after 5 consecutive failed attempts
        constexpr int max_failed_attempts = 5;
        if (login_info.failed_logins >= max_failed_attempts) {
            login_info.locked = true;
            BOOST_LOG_SEV(lg, warn) << "Account locked due to too many failed attempts: "
                                    << username;
        }

        // Update the login_info table with failed attempt
        login_info_repo_.update(ctx, login_info);

        throw std::runtime_error("Invalid username or password");
    }

    // Successful login - update tracking information
    login_info.last_ip = ip_address;
    login_info.last_login = std::chrono::system_clock::now();
    login_info.failed_logins = 0; // Reset failed login counter
    login_info.online = true;

    BOOST_LOG_SEV(lg, info) << "Successful login for username: " << username
                            << " from IP: " << ip_address;

    // Update the login_info table
    login_info_repo_.update(ctx, login_info);

    return account;
}

}
