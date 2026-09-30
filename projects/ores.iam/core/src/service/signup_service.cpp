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
#include "ores.iam.core/service/signup_service.hpp"
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.security/crypto/password_hasher.hpp"
#include "ores.security/validation/email_validator.hpp"
#include "ores.security/validation/password_validator.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::service {

using namespace ores::logging;
using error_code = ores::utility::serialization::error_code;
namespace crypto = ores::security::crypto;
namespace validation = ores::security::validation;
namespace reason = ores::dq::domain::change_reason_constants;

signup_service::signup_service(
    database::context ctx,
    std::shared_ptr<variability::service::system_settings_service> system_flags,
    std::shared_ptr<authorization_service> auth_service)
    : ctx_(ctx)
    , system_flags_(std::move(system_flags))
    , auth_service_(std::move(auth_service)) {}

signup_result signup_service::register_user(const std::string& username,
                                            const std::string& email,
                                            const std::string& password,
                                            const signup_destination& destination) {

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
        BOOST_LOG_SEV(lg(), warn) << "Signup rejected: authorization workflow not implemented";
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
    auto existing_by_username = account_repo_.read_latest_by_username(ctx_, username);
    if (!existing_by_username.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Signup rejected: username already taken: " << username;
        result.error_message = "Username is already taken";
        result.error_code = error_code::username_taken;
        return result;
    }

    // Validate email format
    auto email_validation = validation::email_validator::validate(email);
    if (!email_validation.is_valid) {
        BOOST_LOG_SEV(lg(), warn) << "Signup rejected: invalid email format: " << email;
        result.error_message = email_validation.error_message;
        result.error_code = error_code::invalid_request;
        return result;
    }

    // Check email uniqueness
    auto existing_by_email = account_repo_.read_latest_by_email(ctx_, email);
    if (!existing_by_email.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Signup rejected: email already in use: " << email;
        result.error_message = "Email address is already registered";
        result.error_code = error_code::email_taken;
        return result;
    }

    // Validate password policy
    auto password_validation = validation::password_validator::validate(password);
    if (!password_validation.is_valid) {
        BOOST_LOG_SEV(lg(), warn) << "Signup rejected: weak password";
        result.error_message = password_validation.error_message;
        result.error_code = error_code::weak_password;
        return result;
    }

    /*
     * A tenant that nominates no role cannot admit a registration: the
     * account would hold nothing. The policy read refuses this before the
     * form is offered, and the service refuses it again here, because a
     * service must refuse for every caller and not only for the door.
     */
    if (!destination.role_id) {
        BOOST_LOG_SEV(lg(), warn) << "Signup rejected: the tenant nominates no default role";
        result.error_message = "This deployment has not nominated a role for new accounts.";
        result.error_code = error_code::no_default_role;
        return result;
    }

    /*
     * The account's state follows the tenant's nominations. A nominated party
     * gives the account somewhere to work, so it is usable at once. No party
     * leaves it pending: the account exists, and an administrator finishes
     * setting it up. The roster finds the waiting accounts by this status.
     */
    const std::string account_status = destination.party_id ? "active" : "pending";

    // Generate account ID
    auto id = uuid_generator_();
    BOOST_LOG_SEV(lg(), debug) << "Generated ID for new account: " << id;

    // Hash password
    auto password_hash = crypto::password_hasher::hash(password);

    // Create the account
    // Note: Admin privileges are now managed via RBAC role assignments
    domain::account new_account;
    new_account.version = 0;
    new_account.id = id;
    new_account.username = username;
    new_account.password_hash = password_hash;
    // The hash carries its own salt, so nothing reads this column; the model
    // still declares it and it is written empty.
    new_account.password_salt = "";
    new_account.totp_secret = "";
    new_account.email = email;
    new_account.account_status = account_status;
    new_account.change_reason_code = std::string{reason::codes::new_record};
    // Self-registered: the user's own username records the author.
    new_account.modified_by = username;

    std::vector<domain::account> accounts{new_account};
    account_repo_.write(ctx_, accounts);

    // Create login tracking entry
    domain::login_info li{.account_id = id,
                          .last_ip = {},
                          .last_attempt_ip = {},
                          .failed_logins = 0,
                          .locked = false,
                          .last_login = {},
                          .online = false,
                          .password_reset_required = false};

    std::vector<domain::login_info> login_infos{li};
    login_info_repo_.write(ctx_, login_infos);

    // Grant the role the tenant nominated, which is the account's floor.
    auth_service_->assign_role(id, *destination.role_id, username);
    BOOST_LOG_SEV(lg(), info) << "Assigned the registration default role to new account: " << id;

    // Give the account its first association, when the tenant nominated one.
    if (destination.party_id) {
        domain::account_party link;
        link.version = 0;
        link.tenant_id = ctx_.tenant_id().to_string();
        link.account_id = id;
        link.party_id = *destination.party_id;
        link.modified_by = username;
        link.performed_by = username;
        link.change_reason_code = std::string{reason::codes::new_record};
        link.change_commentary = "Self-registration association";
        repository::account_party_repository ap_repo(ctx_);
        ap_repo.write(link);
        BOOST_LOG_SEV(lg(), info) << "Associated new account " << id << " with party "
                                  << boost::uuids::to_string(*destination.party_id);
    }

    BOOST_LOG_SEV(lg(), info) << "Signup successful for username: " << username
                              << ", account ID: " << id << ", status: " << account_status;

    result.success = true;
    result.account_id = id;
    result.account_status = account_status;
    result.party_id = destination.party_id;
    result.role_id = destination.role_id;
    return result;
}

bool signup_service::is_signup_enabled() const {
    return system_flags_->is_user_signups_enabled();
}

}
