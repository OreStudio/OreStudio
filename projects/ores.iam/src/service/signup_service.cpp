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
#include "ores.iam/service/signup_service.hpp"

#include <boost/uuid/uuid_io.hpp>
#include "ores.iam/domain/role.hpp"
#include "ores.iam/security/password_manager.hpp"
#include "ores.iam/security/password_policy_validator.hpp"
#include "ores.iam/security/email_validator.hpp"

namespace ores::iam::service {

using namespace ores::telemetry::log;
using error_code = ores::comms::messaging::error_code;

signup_service::signup_service(database::context ctx,
    std::shared_ptr<variability::service::system_flags_service> system_flags,
    std::shared_ptr<authorization_service> auth_service)
    : account_repo_(ctx),
      login_info_repo_(ctx),
      system_flags_(std::move(system_flags)),
      auth_service_(std::move(auth_service)) {}

signup_result signup_service::register_user(const std::string& username,
    const std::string& email, const std::string& password) {

    BOOST_LOG_SEV(lg(), info) << "Signup attempt for username: " << username
                              << ", email: " << email;

    signup_result result;
    result.username = username;

    // Check if signups are enabled
    if (!system_flags_->is_user_signups_enabled()) {
        BOOST_LOG_SEV(lg(), warn) << "Signup rejected: signups are disabled";
        result.error_message = "User registration is currently disabled";
        result.error_code = error_code::signup_disabled;
        return result;
    }

    // Check if authorization is required (not yet implemented)
    if (system_flags_->is_signup_requires_authorization_enabled()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Signup rejected: authorization workflow not implemented";
        result.error_message = "Signup authorization workflow is not yet "
            "implemented. Please contact an administrator to create an account.";
        result.error_code = error_code::signup_requires_authorization;
        return result;
    }

    // Validate username is not empty
    if (username.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Signup rejected: empty username";
        result.error_message = "Username cannot be empty";
        result.error_code = error_code::invalid_request;
        return result;
    }

    // Check username uniqueness
    auto existing_by_username = account_repo_.read_latest_by_username(username);
    if (!existing_by_username.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Signup rejected: username already taken: "
                                  << username;
        result.error_message = "Username is already taken";
        result.error_code = error_code::username_taken;
        return result;
    }

    // Validate email format
    using security::email_validator;
    auto email_validation = email_validator::validate(email);
    if (!email_validation.is_valid) {
        BOOST_LOG_SEV(lg(), warn) << "Signup rejected: invalid email format: "
                                  << email;
        result.error_message = email_validation.error_message;
        result.error_code = error_code::invalid_request;
        return result;
    }

    // Check email uniqueness
    auto existing_by_email = account_repo_.read_latest_by_email(email);
    if (!existing_by_email.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Signup rejected: email already in use: "
                                  << email;
        result.error_message = "Email address is already registered";
        result.error_code = error_code::email_taken;
        return result;
    }

    // Validate password policy
    using security::password_policy_validator;
    auto password_validation = password_policy_validator::validate(password);
    if (!password_validation.is_valid) {
        BOOST_LOG_SEV(lg(), warn) << "Signup rejected: weak password";
        result.error_message = password_validation.error_message;
        result.error_code = error_code::weak_password;
        return result;
    }

    // Generate account ID
    auto id = uuid_generator_();
    BOOST_LOG_SEV(lg(), debug) << "Generated ID for new account: " << id;

    // Hash password
    using security::password_manager;
    auto password_hash = password_manager::create_password_hash(password);

    // Create the account
    // Note: Admin privileges are now managed via RBAC role assignments
    domain::account new_account {
        .version = 0,
        .id = id,
        .recorded_by = username,  // Self-registered
        .username = username,
        .password_hash = password_hash,
        .password_salt = "", // FIXME remove
        .totp_secret = "",
        .email = email
    };

    std::vector<domain::account> accounts{new_account};
    account_repo_.write(accounts);

    // Create login tracking entry
    domain::login_info li {
        .last_login = {},
        .account_id = id,
        .failed_logins = 0,
        .locked = false,
        .online = false,
        .password_reset_required = false,
        .last_ip = {},
        .last_attempt_ip = {}
    };

    std::vector<domain::login_info> login_infos{li};
    login_info_repo_.write(login_infos);

    // Assign the default Viewer role to the new account
    auto viewer_role = auth_service_->find_role_by_name(domain::roles::viewer);
    if (!viewer_role) {
        BOOST_LOG_SEV(lg(), error)
            << "Viewer role not found - RBAC may not be properly seeded. "
            << "Signup failed for account " << id;
        throw std::runtime_error(
            "Default 'Viewer' role not found. Cannot create account without "
            "default permissions.");
    }

    auth_service_->assign_role(id, viewer_role->id, username);
    BOOST_LOG_SEV(lg(), info) << "Assigned Viewer role to new account: " << id;

    BOOST_LOG_SEV(lg(), info) << "Signup successful for username: " << username
                              << ", account ID: " << id;

    result.success = true;
    result.account_id = id;
    return result;
}

bool signup_service::is_signup_enabled() const {
    return system_flags_->is_user_signups_enabled();
}

}
