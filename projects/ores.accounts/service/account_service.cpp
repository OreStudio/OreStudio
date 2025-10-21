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
    repository::logins_repository logins_repo)
    : account_repo_(std::move(account_repo)),
      logins_repo_(std::move(logins_repo)) {
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
    domain::logins login_info{
        .account_id = id,
        .last_ip = {},
        .last_attempt_ip = {},
        .failed_logins = 0,
        .locked = false,
        .last_login = {},
        .online = false
    };

    std::vector<domain::logins> logins{login_info};
    logins_repo_.write(ctx, logins);

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

}
