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
#include "ores.iam.core/service/account_operations_service.hpp"
#include "ores.dq.api/domain/change_reason_constants.hpp"
#include "ores.iam.core/repository/tenant_lookups.hpp"
#include "ores.security/crypto/password_hasher.hpp"
#include "ores.security/validation/email_validator.hpp"
#include "ores.security/validation/password_validator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <queue>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <array>
#include <format>
#include <limits>
#include <openssl/evp.h>
#include <stdexcept>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace ores::iam::service {

using namespace ores::logging;
using ores::service::messaging::stamp;
namespace reason = ores::dq::domain::change_reason_constants;
namespace crypto = ores::security::crypto;
namespace validation = ores::security::validation;
using error_code = ores::utility::serialization::error_code;

void account_operations_service::throw_if_empty(const std::string& name, const std::string& value) {
    BOOST_LOG_SEV(lg(), debug) << name << ": '" << value << "'";
    if (value.empty()) {
        BOOST_LOG_SEV(lg(), error) << name << " cannot be empty.";
        throw std::invalid_argument(name + " cannot be empty.");
    }
}

account_operations_service::account_operations_service(database::context ctx)
    : ctx_(ctx) {

    BOOST_LOG_SEV(lg(), debug) << "DML for account: " << account_repo_.sql();
    BOOST_LOG_SEV(lg(), debug) << "DML for account_credential: " << credential_repo_.sql();
    BOOST_LOG_SEV(lg(), debug) << "DML for login_info: " << login_info_repo_.sql();
}

std::optional<domain::account_credential>
account_operations_service::read_credential(const boost::uuids::uuid& account_id) {
    auto rows =
        credential_repo_.read_latest_by_account_id(ctx_, boost::uuids::to_string(account_id), 0, 1);
    if (rows.empty())
        return std::nullopt;
    return rows.front();
}

void account_operations_service::write_credential(domain::account_credential credential) {
    std::vector<domain::account_credential> rows{std::move(credential)};
    credential_repo_.write(ctx_, rows);
}

std::optional<boost::uuids::uuid>
account_operations_service::verify_service_credentials(const std::string& username,
                                                       const std::string& password) {
    BOOST_LOG_SEV(lg(), debug) << "Checking service credentials for: " << username;

    const auto accounts = account_repo_.read_latest_by_username(ctx_, username);
    if (accounts.empty())
        return std::nullopt;

    const auto& account = accounts.front();
    if (account.account_type == "user") {
        BOOST_LOG_SEV(lg(), debug) << "Rejecting user account for service login: " << username;
        return std::nullopt;
    }

    const auto credential = read_credential(account.id);
    if (!credential) {
        // The account is a service account with no credential row at all,
        // which is a data fault and not a rejected password. Say which, so
        // the two are not read as one another.
        BOOST_LOG_SEV(lg(), warn)
            << "Service account holds no credential row, so it cannot authenticate: " << username;
        return std::nullopt;
    }

    if (credential->service_password_hash.get().empty()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Service account's credential holds no password hash, so it cannot authenticate: "
            << username;
        return std::nullopt;
    }

    std::array<unsigned char, EVP_MAX_MD_SIZE> digest{};
    unsigned int digest_len = 0;
    EVP_Digest(password.data(), password.size(), digest.data(), &digest_len, EVP_sha256(), nullptr);

    std::string computed_hash;
    computed_hash.reserve(digest_len * 2);
    for (unsigned int i = 0; i < digest_len; ++i)
        computed_hash += std::format("{:02x}", digest[i]);

    if (computed_hash != credential->service_password_hash.get()) {
        BOOST_LOG_SEV(lg(), debug) << "Password mismatch for service account: " << username;
        return std::nullopt;
    }

    return account.id;
}

domain::account account_operations_service::create_account(const std::string& username,
                                                           const std::string& email,
                                                           const std::string& password,
                                                           const std::string& modified_by,
                                                           const std::string& change_commentary,
                                                           const std::string& full_name) {

    throw_if_empty("Username", username);
    throw_if_empty("Email", email);
    // FIXME: do not log
    throw_if_empty("Password", password);

    // Generate a new UUID for the account
    boost::uuids::random_generator gen;
    auto id = uuid_generator_();
    BOOST_LOG_SEV(lg(), debug) << "ID for new account: " << id;

    // Hash the password
    auto password_hash = crypto::password_hasher::hash(password);

    // Create the account object with computed fields
    // Note: Administrative privileges are now managed through RBAC roles.
    domain::account new_account;
    // The insert trigger sets the version.
    new_account.version = 0;
    new_account.id = id;
    new_account.username = username;
    new_account.full_name = full_name;
    new_account.account_type = "user";
    new_account.email = email;
    new_account.modified_by = modified_by;
    new_account.change_reason_code = std::string{reason::codes::new_record};
    new_account.change_commentary = change_commentary;

    std::vector<domain::account> accounts{new_account};
    account_repo_.write(ctx_, accounts);

    // The password is the account's credential, which is the one place it is
    // held: a later write through the account cannot reach it.
    domain::account_credential credential;
    credential.version = 0;
    credential.id = uuid_generator_();
    credential.account_id = id;
    credential.password_hash = password_hash;
    credential.modified_by = modified_by;
    credential.change_reason_code = std::string{reason::codes::new_record};
    credential.change_commentary = change_commentary;
    write_credential(std::move(credential));

    // Create a corresponding login tracking entry
    domain::login_info li{.account_id = id,
                          .last_ip = {},
                          .last_attempt_ip = {},
                          .failed_logins = 0,
                          .locked = false,
                          .last_login = {},
                          .online = false};

    std::vector<domain::login_info> login_infos{li};
    login_info_repo_.write(ctx_, login_infos);

    return new_account;
}

domain::account
account_operations_service::create_service_account(const std::string& username,
                                                   const std::string& email,
                                                   const std::string& account_type,
                                                   const std::string& modified_by,
                                                   const std::string& change_commentary) {

    throw_if_empty("Username", username);
    throw_if_empty("Email", email);
    throw_if_empty("Account type", account_type);

    // Validate account type is not 'user'
    if (account_type == "user") {
        BOOST_LOG_SEV(lg(), error) << "Cannot create user account via create_service_account. "
                                   << "Use create_account() instead.";
        throw std::invalid_argument("Use create_account() for user accounts");
    }

    // Validate account type is one of the valid service account types
    if (account_type != "service" && account_type != "algorithm" && account_type != "llm") {
        BOOST_LOG_SEV(lg(), error) << "Invalid account type for service account: " << account_type;
        throw std::invalid_argument("Account type must be 'service', 'algorithm', or 'llm'");
    }

    // Generate a new UUID for the account
    auto id = uuid_generator_();
    BOOST_LOG_SEV(lg(), debug) << "ID for new service account: " << id;

    // Create the service account - no password required
    domain::account new_account;
    // The insert trigger sets the version.
    new_account.version = 0;
    new_account.id = id;
    new_account.username = username;
    new_account.account_type = account_type;
    new_account.email = email;
    new_account.modified_by = modified_by;
    new_account.change_reason_code = std::string{reason::codes::new_record};
    new_account.change_commentary = change_commentary;

    std::vector<domain::account> accounts{new_account};
    account_repo_.write(ctx_, accounts);

    // A service account authenticates by proving a machine password, so it
    // holds a credential row the moment it exists. The row is empty until the
    // seed writes the service hash: an account with no credential row at all
    // is the fault the service login path reports as one.
    domain::account_credential credential;
    credential.version = 0;
    credential.id = uuid_generator_();
    credential.account_id = id;
    credential.modified_by = modified_by;
    credential.change_reason_code = std::string{reason::codes::new_record};
    credential.change_commentary = change_commentary;
    write_credential(std::move(credential));

    // Create a corresponding login tracking entry (for consistency)
    domain::login_info li{.account_id = id,
                          .last_ip = {},
                          .last_attempt_ip = {},
                          .failed_logins = 0,
                          .locked = false,
                          .last_login = {},
                          .online = false};

    std::vector<domain::login_info> login_infos{li};
    login_info_repo_.write(ctx_, login_infos);

    BOOST_LOG_SEV(lg(), info) << "Created service account: " << username
                              << " (type: " << account_type << ")";

    return new_account;
}

std::optional<domain::account>
account_operations_service::get_account(const boost::uuids::uuid& account_id) {
    auto accounts = account_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (accounts.empty()) {
        return std::nullopt;
    }
    return accounts[0];
}

std::vector<domain::account> account_operations_service::list_accounts() {
    return account_repo_.read_latest(ctx_);
}

std::vector<domain::account> account_operations_service::list_accounts(std::uint32_t offset,
                                                                       std::uint32_t limit) {
    return account_repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t account_operations_service::get_total_account_count() {
    return account_repo_.get_total_account_count(ctx_);
}

std::vector<domain::login_info> account_operations_service::list_login_info() {
    return login_info_repo_.read_latest(ctx_);
}

void account_operations_service::delete_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Deleting account: " << boost::uuids::to_string(account_id);

    // Verify account exists before attempting deletion
    auto accounts = account_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to delete non-existent account: "
                                  << boost::uuids::to_string(account_id);
        throw std::invalid_argument("Account does not exist");
    }

    // Perform bitemporal soft delete (sets valid_to = current_timestamp)
    account_repo_.remove(ctx_, boost::uuids::to_string(account_id));

    BOOST_LOG_SEV(lg(), info) << "Successfully deleted account: "
                              << boost::uuids::to_string(account_id);
}

authenticated_login account_operations_service::login(const std::string& username,
                                                      const std::string& password,
                                                      const boost::asio::ip::address& ip_address) {

    throw_if_empty("Username", username);
    // FIXME: do not log
    throw_if_empty("Password", password);

    BOOST_LOG_SEV(lg(), debug) << "Login attempt for username: " << username
                               << " from IP: " << ip_address;

    // Read the account by username
    auto accounts = account_repo_.read_latest_by_username(ctx_, username);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Login failed: account not found for username: " << username;
        throw login_error(error_code::invalid_credentials, "Invalid username or password");
    }

    const auto& account = accounts[0];

    // A suspended or terminated tenant admits nobody. The check runs before
    // the password is compared, so the person learns the tenant is closed
    // rather than reading a wrong-password message. It reads the status of the
    // account's own tenant, not the caller context's: the account read matches
    // a username across tenants, and a login with no resolvable hostname stays
    // on the handler's system-tenant context, which is always active.
    const auto tenants = repository::read_active_tenant_by_id(ctx_, account.tenant_id.to_uuid());
    if (tenants.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Login failed: tenant not found for username: " << username;
        throw login_error(error_code::invalid_credentials, "Invalid username or password");
    }
    const auto& tenant_status = tenants.front().status;
    if (!repository::admits_sign_in(tenant_status)) {
        BOOST_LOG_SEV(lg(), warn) << "Login refused for a tenant in status '" << tenant_status
                                  << "' for username: " << username;
        throw login_error(error_code::tenant_inactive, "Tenant is not active");
    }

    // Only user accounts can login with password
    if (account.account_type != "user") {
        BOOST_LOG_SEV(lg(), warn) << "Login attempt for non-user account type '"
                                  << account.account_type << "' for username: " << username;
        throw std::runtime_error("Password login is only available for user accounts");
    }

    auto login_info_vec = login_info_repo_.read_latest(ctx_, boost::uuids::to_string(account.id));
    if (login_info_vec.empty()) {
        BOOST_LOG_SEV(lg(), error)
            << "Login tracking not found for account: " << boost::uuids::to_string(account.id);
        throw std::runtime_error("Login tracking information missing");
    }

    auto login_info = login_info_vec[0];

    if (login_info.locked) {
        BOOST_LOG_SEV(lg(), warn) << "Login attempt for locked account: " << username;
        throw login_error(error_code::account_locked,
                          "Account is locked due to too many failed attempts");
    }

    const auto credential = read_credential(account.id);
    if (!credential || credential->password_hash.get().empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Login refused: account holds no password credential: "
                                  << username;
        throw login_error(error_code::invalid_credentials, "Invalid username or password");
    }

    bool password_valid =
        crypto::password_hasher::verify(password, credential->password_hash.get());

    login_info.last_attempt_ip = ip_address;

    if (!password_valid) {
        login_info.failed_logins++;
        BOOST_LOG_SEV(lg(), warn) << "Failed login attempt for username: " << username
                                  << ". Attempt: " << login_info.failed_logins;

        login_info_repo_.write(ctx_, login_info);

        constexpr int max_failed_attempts = 5;
        if (login_info.failed_logins >= max_failed_attempts) {
            lock_account(account.id);
            BOOST_LOG_SEV(lg(), warn)
                << "Account locked due to too many failed attempts: " << username;
        }

        throw login_error(error_code::invalid_credentials, "Invalid username or password");
    }

    login_info.last_ip = ip_address;
    login_info.last_login = std::chrono::system_clock::now();
    login_info.failed_logins = 0;
    login_info.online = true;

    BOOST_LOG_SEV(lg(), info) << "Successful login for username: " << username
                              << " from IP: " << ip_address;

    login_info_repo_.write(ctx_, login_info);

    return authenticated_login{.account = account,
                               .password_reset_required = login_info.password_reset_required};
}

bool account_operations_service::lock_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Locking account: " << boost::uuids::to_string(account_id);

    auto accounts = account_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to lock non-existent account: "
                                  << boost::uuids::to_string(account_id);
        return false;
    }

    auto login_info_vec = login_info_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (login_info_vec.empty()) {
        BOOST_LOG_SEV(lg(), error)
            << "Login tracking not found for account: " << boost::uuids::to_string(account_id);
        return false;
    }

    auto login_info = login_info_vec[0];

    if (login_info.locked == true) {
        BOOST_LOG_SEV(lg(), warn) << "Account is already locked.";
        return true;
    }

    login_info.locked = true;

    BOOST_LOG_SEV(lg(), info) << "Account locked: " << boost::uuids::to_string(account_id);

    login_info_repo_.write(ctx_, login_info);
    return true;
}

bool account_operations_service::unlock_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Unlocking account: " << boost::uuids::to_string(account_id);

    auto accounts = account_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to unlock non-existent account: "
                                  << boost::uuids::to_string(account_id);
        return false;
    }

    auto login_info_vec = login_info_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (login_info_vec.empty()) {
        BOOST_LOG_SEV(lg(), error)
            << "Login tracking not found for account: " << boost::uuids::to_string(account_id);
        return false;
    }

    auto login_info = login_info_vec[0];

    if (login_info.locked == false) {
        BOOST_LOG_SEV(lg(), warn) << "Account is not locked.";
        return true;
    }

    login_info.locked = false;
    login_info.failed_logins = 0;

    BOOST_LOG_SEV(lg(), info) << "Account unlocked: " << boost::uuids::to_string(account_id);

    login_info_repo_.write(ctx_, login_info);
    return true;
}

void account_operations_service::logout(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Logging out account: " << boost::uuids::to_string(account_id);

    auto accounts = account_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to logout non-existent account: "
                                  << boost::uuids::to_string(account_id);
        throw std::invalid_argument("Account does not exist");
    }

    auto login_info_vec = login_info_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (login_info_vec.empty()) {
        BOOST_LOG_SEV(lg(), error)
            << "Login tracking not found for account: " << boost::uuids::to_string(account_id);
        throw std::runtime_error("Login tracking information missing");
    }

    auto login_info = login_info_vec[0];
    login_info.online = false;

    BOOST_LOG_SEV(lg(), info) << "Account logged out: " << boost::uuids::to_string(account_id);

    login_info_repo_.write(ctx_, login_info);
}

bool account_operations_service::update_account(
    const boost::uuids::uuid& account_id,
    const std::string& email,
    const std::string& full_name,
    const std::optional<boost::uuids::uuid>& default_party_id,
    const std::string& job_title,
    const boost::uuids::uuid& reports_to_account_id,
    const boost::uuids::uuid& image_id,
    const std::string& modified_by,
    const std::string& change_reason_code,
    const std::string& change_commentary) {
    BOOST_LOG_SEV(lg(), debug) << "Updating account: " << boost::uuids::to_string(account_id);

    // Verify account exists
    auto accounts = account_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to update non-existent account: "
                                  << boost::uuids::to_string(account_id);
        throw std::invalid_argument("Account does not exist");
    }

    // Get existing account and create new version with updated fields
    auto account = accounts[0];
    account.email = email;
    account.full_name = full_name;
    account.default_party_id = default_party_id;
    account.job_title = job_title;
    account.reports_to_account_id =
        reports_to_account_id.is_nil() ? std::nullopt : std::optional(reports_to_account_id);
    account.image_id = image_id.is_nil() ? std::nullopt : std::optional(image_id);
    account.modified_by = modified_by;
    account.change_reason_code = change_reason_code;
    account.change_commentary = change_commentary;
    // Note: version is NOT incremented here - the database trigger handles it
    // The trigger uses optimistic locking: new.version must match current_version

    // Write the updated account (creates new temporal version)
    account_repo_.write(ctx_, account);

    BOOST_LOG_SEV(lg(), info) << "Successfully updated account: "
                              << boost::uuids::to_string(account_id)
                              << ", new version: " << account.version;

    return true;
}

messaging::update_self_account_response account_operations_service::update_self_account(
    const messaging::update_self_account_request& request, const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Updating own account: " << boost::uuids::to_string(account_id);
    messaging::update_self_account_response response;

    // The three fields only an administrator owns are declared on the request
    // so a stated value is refused by name, never dropped in silence. An empty
    // string is not a stated value: it means the field is not stated.
    const auto refuse_unowned = [&response](std::string_view field, const std::string& value) {
        if (value.empty())
            return;
        response.result.fields.push_back({std::string(field),
                                          "field_not_self_writable",
                                          "Only an administrator can change this field."});
    };
    refuse_unowned("email", request.email);
    refuse_unowned("default_party_id", request.default_party_id);
    refuse_unowned("reports_to_account_id", request.reports_to_account_id);
    if (!response.result.fields.empty()) {
        response.result.outcome = ores::utility::domain::outcome::denied;
        response.result.code = "field_not_self_writable";
        response.result.message = "A member cannot change every field the request states.";
        return response;
    }

    auto accounts = account_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (accounts.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        response.result.message = "Account does not exist.";
        return response;
    }

    std::optional<boost::uuids::uuid> image_id;
    if (!request.image_id.empty()) {
        try {
            boost::uuids::string_generator sg;
            image_id = sg(request.image_id);
        } catch (const std::exception&) {
            response.result.outcome = ores::utility::domain::outcome::invalid;
            response.result.code = "invalid_image_id";
            response.result.fields.push_back(
                {"image_id", "invalid_image_id", "image_id is not a UUID."});
            return response;
        }
    }

    auto account = accounts[0];
    account.full_name = request.full_name;
    account.job_title = request.job_title;
    account.image_id = image_id;
    account.change_reason_code = request.change_reason_code;
    account.change_commentary = request.change_commentary;
    stamp(account, ctx_, ores::service::messaging::change_reasons::update);
    account_repo_.write(ctx_, account);

    auto written = account_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (!written.empty())
        response.account = written.front();

    BOOST_LOG_SEV(lg(), info) << "Updated own account: " << boost::uuids::to_string(account_id);
    return response;
}

messaging::update_self_account_contact_information_response
account_operations_service::update_self_account_contact_information(
    const messaging::update_self_account_contact_information_request& request,
    const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Updating own account contact information: "
                               << boost::uuids::to_string(account_id);
    messaging::update_self_account_contact_information_response response;

    // A stated value must be one the record can hold: a contact email that is a
    // real address, and a country that is an ISO 3166-1 alpha-2 code. A stated
    // value that fails leaves the record as it is.
    const auto refuse_invalid =
        [&response](std::string_view field, bool valid, std::string_view message) {
            if (!valid)
                response.result.fields.push_back(
                    {std::string(field), "invalid_field_value", std::string(message)});
        };
    if (!request.email.empty())
        refuse_invalid("email",
                       validation::email_validator::validate(request.email).is_valid,
                       "The email is not a valid address.");
    if (!request.country_code.empty())
        refuse_invalid("country_code",
                       request.country_code.size() == 2,
                       "The country code must be an ISO 3166-1 alpha-2 code.");
    if (!response.result.fields.empty()) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "invalid_field_value";
        response.result.message = "The request states a value the record cannot hold.";
        return response;
    }

    const auto account_id_text = boost::uuids::to_string(account_id);
    auto existing = contact_repo_.read_latest_by_account_id(ctx_, account_id_text, 0, 1);
    domain::account_contact_information record;
    if (existing.empty()) {
        // One contact record per account, so a member who has none gets one
        // here rather than being told to ask an administrator to create it.
        record.id = uuid_generator_();
        record.account_id = account_id;
    } else {
        record = existing.front();
    }
    record.street_line_1 = request.street_line_1;
    record.street_line_2 = request.street_line_2;
    record.city = request.city;
    record.state = request.state;
    record.country_code = request.country_code;
    record.postal_code = request.postal_code;
    record.phone = request.phone;
    record.email = request.email;
    record.web_page = request.web_page;
    record.change_reason_code = request.change_reason_code;
    record.change_commentary = request.change_commentary;
    stamp(record, ctx_, ores::service::messaging::change_reasons::update);
    contact_repo_.write(ctx_, record);

    auto written = contact_repo_.read_latest_by_account_id(ctx_, account_id_text, 0, 1);
    if (!written.empty())
        response.account_contact_information = written.front();

    BOOST_LOG_SEV(lg(), info) << "Updated own account contact information: " << account_id_text;
    return response;
}

messaging::get_my_account_contact_information_response
account_operations_service::get_my_account_contact_information(
    const boost::uuids::uuid& account_id) {
    const auto account_id_text = boost::uuids::to_string(account_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading own account contact information: " << account_id_text;
    messaging::get_my_account_contact_information_response response;
    auto records = contact_repo_.read_latest_by_account_id(ctx_, account_id_text, 0, 1);
    if (!records.empty())
        response.account_contact_information = records.front();
    return response;
}

std::optional<domain::account>
account_operations_service::find_account_by_username(const std::string& username) {
    BOOST_LOG_SEV(lg(), debug) << "Finding account by username: " << username;

    auto accounts = account_repo_.read_latest_by_username(ctx_, username);
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "No account found for username: " << username;
        return std::nullopt;
    }
    return accounts.front();
}

std::optional<domain::account>
account_operations_service::find_account_by_id(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding account by id: " << boost::uuids::to_string(account_id);

    auto accounts = account_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), debug)
            << "No account found for id: " << boost::uuids::to_string(account_id);
        return std::nullopt;
    }
    return accounts.front();
}

bool account_operations_service::set_password_reset_required(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Setting password_reset_required for account: "
                               << boost::uuids::to_string(account_id);

    auto login_info_vec = login_info_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (login_info_vec.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Account or login tracking not found for account: "
                                  << boost::uuids::to_string(account_id);
        return false;
    }

    auto login_info = login_info_vec[0];

    if (login_info.password_reset_required) {
        BOOST_LOG_SEV(lg(), debug) << "Password reset is already required for account.";
        return true;
    }

    login_info.password_reset_required = true;

    BOOST_LOG_SEV(lg(), info) << "Password reset required set for account: "
                              << boost::uuids::to_string(account_id);

    login_info_repo_.write(ctx_, login_info);
    return true;
}

std::string account_operations_service::change_password(const boost::uuids::uuid& account_id,
                                                        const std::string& current_password,
                                                        const std::string& new_password) {

    if (current_password.empty()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Password change refused: no current password supplied for account: "
            << boost::uuids::to_string(account_id);
        return "Current password is required";
    }

    auto accounts = account_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to change password for non-existent account: "
                                  << boost::uuids::to_string(account_id);
        return "Account does not exist";
    }

    const auto credential = read_credential(account_id);
    if (!credential || credential->password_hash.get().empty()) {
        BOOST_LOG_SEV(lg(), warn)
            << "Password change refused: account holds no password credential: "
            << boost::uuids::to_string(account_id);
        return "Current password is incorrect";
    }

    if (!crypto::password_hasher::verify(current_password, credential->password_hash.get())) {
        BOOST_LOG_SEV(lg(), warn)
            << "Password change refused: current password does not match for account: "
            << boost::uuids::to_string(account_id);
        return "Current password is incorrect";
    }

    return reset_password(account_id, new_password);
}

std::string account_operations_service::reset_password(const boost::uuids::uuid& account_id,
                                                       const std::string& new_password) {
    BOOST_LOG_SEV(lg(), debug) << "Changing password for account: "
                               << boost::uuids::to_string(account_id);

    // Validate password strength using policy validator
    auto pass_validation = validation::password_validator::validate(new_password);
    if (!pass_validation.is_valid) {
        return pass_validation.error_message;
    }

    // Verify account exists
    auto accounts = account_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to change password for non-existent account: "
                                  << boost::uuids::to_string(account_id);
        return "Account does not exist";
    }

    const auto existing = read_credential(account_id);

    // Check that new password is different from current password
    if (existing && !existing->password_hash.get().empty() &&
        crypto::password_hasher::verify(new_password, existing->password_hash.get())) {
        BOOST_LOG_SEV(lg(), debug) << "New password matches current password";
        return "New password must be different from current password";
    }

    // A password change is a credential write and nothing else. The account
    // row is not touched: the profile is not what changed, and a write through
    // it could not reach a credential in any case.
    auto credential = existing.value_or(domain::account_credential{});
    credential.id = existing ? existing->id : uuid_generator_();
    credential.account_id = account_id;
    credential.password_hash = crypto::password_hasher::hash(new_password);
    credential.modified_by = existing && !existing->modified_by.empty() ? existing->modified_by :
                                                                          accounts.front().username;
    credential.change_reason_code = std::string{reason::codes::non_material_update};
    credential.change_commentary = "Password changed";

    // Write the credential (creates a new temporal version of its own row)
    write_credential(std::move(credential));

    // Clear password_reset_required flag
    auto login_info_vec = login_info_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (!login_info_vec.empty()) {
        auto login_info = login_info_vec[0];
        if (login_info.password_reset_required) {
            login_info.password_reset_required = false;
            login_info_repo_.write(ctx_, login_info);
            BOOST_LOG_SEV(lg(), debug) << "Cleared password_reset_required flag";
        }
    }

    BOOST_LOG_SEV(lg(), info) << "Successfully changed password for account: "
                              << boost::uuids::to_string(account_id);

    // An empty string indicates success.
    return "";
}

domain::login_info
account_operations_service::get_login_info(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting login_info for account: "
                               << boost::uuids::to_string(account_id);

    auto login_info_vec = login_info_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (login_info_vec.empty()) {
        BOOST_LOG_SEV(lg(), error)
            << "Login tracking not found for account: " << boost::uuids::to_string(account_id);
        throw std::runtime_error("Login tracking information missing");
    }

    return login_info_vec[0];
}

std::string account_operations_service::update_my_email(const boost::uuids::uuid& account_id,
                                                        const std::string& new_email) {
    BOOST_LOG_SEV(lg(), debug) << "Updating email for account: "
                               << boost::uuids::to_string(account_id);

    // Validate email format
    auto email_validation = validation::email_validator::validate(new_email);
    if (!email_validation.is_valid) {
        return email_validation.error_message;
    }

    // Verify account exists
    auto accounts = account_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
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
    account_repo_.write(ctx_, account);

    BOOST_LOG_SEV(lg(), info) << "Successfully updated email for account: "
                              << boost::uuids::to_string(account_id);

    // An empty string indicates success.
    return "";
}

std::string account_operations_service::set_my_default_party(
    const boost::uuids::uuid& account_id, const std::optional<boost::uuids::uuid>& party_id) {
    BOOST_LOG_SEV(lg(), debug) << "Setting default party for account: "
                               << boost::uuids::to_string(account_id);

    auto accounts = account_repo_.read_latest(ctx_, boost::uuids::to_string(account_id));
    if (accounts.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Attempted to set default party for non-existent account: "
                                  << boost::uuids::to_string(account_id);
        return "Account does not exist";
    }

    if (accounts[0].default_party_id == party_id) {
        // Idempotent no-op: re-running "set default to X" when X is already
        // the default should succeed silently, not fail — callers like
        // provisioning scripts and the shell command may legitimately repeat
        // this call against an already-provisioned system. The same holds for
        // clearing a default that is not set.
        BOOST_LOG_SEV(lg(), debug)
            << "Default party unchanged for account: " << boost::uuids::to_string(account_id);
        return "";
    }

    auto account = accounts[0];
    account.default_party_id = party_id;
    account.change_reason_code = std::string{reason::codes::non_material_update};
    account.change_commentary = party_id ? "Default party changed" : "Default party cleared";
    // Note: version is NOT incremented here - the database trigger handles it

    account_repo_.write(ctx_, account);

    BOOST_LOG_SEV(lg(), info) << "Successfully set default party for account: "
                              << boost::uuids::to_string(account_id);

    // An empty string indicates success.
    return "";
}

messaging::set_reporting_line_response account_operations_service::set_reporting_line(
    const messaging::set_reporting_line_request& request) {
    BOOST_LOG_SEV(lg(), debug) << "Setting the reporting line for account: " << request.account_id;

    messaging::set_reporting_line_response response;

    boost::uuids::string_generator sg;
    boost::uuids::uuid account_id;
    try {
        account_id = sg(request.account_id);
    } catch (const std::exception&) {
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "invalid_account_id";
        response.result.message = "The account identifier is not a UUID.";
        return response;
    }

    auto accounts = account_repo_.read_latest(ctx_, request.account_id);
    if (accounts.empty()) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        response.result.message = "No account has this identifier.";
        return response;
    }

    auto account = accounts.front();
    if (!request.expected_version.empty()) {
        int expected = 0;
        try {
            std::size_t consumed = 0;
            expected = std::stoi(request.expected_version, &consumed);
            if (consumed != request.expected_version.size())
                throw std::invalid_argument("trailing characters");
        } catch (const std::exception&) {
            response.result.outcome = ores::utility::domain::outcome::invalid;
            response.result.code = "invalid_expected_version";
            response.result.message = "The expected version is not a whole number.";
            return response;
        }
        if (expected != account.version) {
            response.result.outcome = ores::utility::domain::outcome::conflict;
            response.result.code = "version_conflict";
            response.result.message = "The account has changed since it was read.";
            return response;
        }
    }

    std::optional<boost::uuids::uuid> manager_id;
    if (!request.reports_to_account_id.empty()) {
        try {
            manager_id = sg(request.reports_to_account_id);
        } catch (const std::exception&) {
            response.result.outcome = ores::utility::domain::outcome::invalid;
            response.result.code = "invalid_reports_to_account_id";
            response.result.message = "The manager identifier is not a UUID.";
            return response;
        }
        if (*manager_id == account_id) {
            response.result.outcome = ores::utility::domain::outcome::invalid;
            response.result.code = "self_reporting";
            response.result.message = "An account cannot report to itself.";
            return response;
        }
    }

    // Only the line is written. Every other field keeps the value the account
    // already had, which is the point of a write narrower than update_account.
    account.reports_to_account_id = manager_id;
    account.change_reason_code = request.change_reason_code.empty() ?
                                     std::string{reason::codes::non_material_update} :
                                     request.change_reason_code;
    account.change_commentary = request.change_commentary;

    account_repo_.write(ctx_, account);

    // The store stamps the new version, so the answer states the row as it now
    // is rather than the copy this method read.
    const auto written = account_repo_.read_latest(ctx_, request.account_id);
    response.account = written.empty() ? std::optional<domain::account>{account} :
                                         std::optional<domain::account>{written.front()};
    response.result.outcome = ores::utility::domain::outcome::ok;

    BOOST_LOG_SEV(lg(), info) << "Set the reporting line for account: " << request.account_id;
    return response;
}

messaging::get_reporting_tree_response account_operations_service::get_reporting_tree(
    const messaging::get_reporting_tree_request& request) {
    messaging::get_reporting_tree_response response;

    // The tenant's own roster, which row-level security bounds.
    const auto accounts = account_repo_.read_latest(ctx_);

    std::unordered_map<std::string, const domain::account*> by_id;
    std::unordered_map<std::string, std::vector<std::string>> children;
    for (const auto& account : accounts) {
        const auto id = boost::uuids::to_string(account.id);
        by_id[id] = &account;
        if (account.reports_to_account_id) {
            children[boost::uuids::to_string(*account.reports_to_account_id)].push_back(id);
        }
    }

    // A root is an account that states no manager. An account that names a
    // manager the tenant no longer holds is not promoted to a root: it belongs
    // to the unrooted total, because drawing it as a root would hide the gap.
    std::vector<std::string> roots;
    for (const auto& account : accounts) {
        if (!account.reports_to_account_id) {
            roots.push_back(boost::uuids::to_string(account.id));
        }
    }

    if (!request.root_account_id.empty() && by_id.count(request.root_account_id) == 0) {
        response.result.outcome = ores::utility::domain::outcome::missing;
        response.result.code = "not_found";
        response.result.message = "No account in this tenant has that identifier.";
        return response;
    }

    // Breadth-first from the roots, so the walk is the distance and a ring the
    // store accepted before the guard existed cannot spin: a row is placed
    // once. The walk is over the whole tenant even when one branch is asked
    // for, because a depth and the unrooted total are facts about the tenant
    // rather than about the branch that happened to be read.
    std::unordered_map<std::string, int> depth;
    std::vector<std::string> order;
    std::queue<std::pair<std::string, int>> pending;
    for (const auto& root : roots) {
        depth[root] = 0;
        pending.push({root, 0});
    }
    while (!pending.empty()) {
        const auto front = pending.front();
        pending.pop();
        order.push_back(front.first);
        const auto kids = children.find(front.first);
        if (kids == children.end()) {
            continue;
        }
        for (const auto& child : kids->second) {
            if (depth.count(child) > 0) {
                continue;
            }
            depth[child] = front.second + 1;
            pending.push({child, front.second + 1});
        }
    }

    // What the walk could not reach belongs to no root.
    std::vector<std::string> unplaced;
    for (const auto& account : accounts) {
        const auto id = boost::uuids::to_string(account.id);
        if (depth.count(id) == 0) {
            ++response.unrooted;
            unplaced.push_back(id);
        }
    }

    const auto node_for = [&](const std::string& id, int d) {
        const auto* account = by_id.at(id);
        const auto kids = children.find(id);
        return messaging::reporting_tree_node{
            .account_id = id,
            .username = account->username,
            .full_name = account->full_name,
            .job_title = account->job_title,
            .reports_to_account_id = account->reports_to_account_id ?
                                         boost::uuids::to_string(*account->reports_to_account_id) :
                                         std::string{},
            .depth = d,
            .direct_reports = static_cast<int>(kids == children.end() ? 0 : kids->second.size())};
    };

    if (request.root_account_id.empty()) {
        for (const auto& id : order) {
            response.nodes.push_back(node_for(id, depth.at(id)));
        }
        for (const auto& id : unplaced) {
            response.nodes.push_back(node_for(id, -1));
        }
    } else if (depth.count(request.root_account_id) == 0) {
        // The stated root reaches no root itself, so its branch is that row
        // alone, and it is already counted above.
        response.nodes.push_back(node_for(request.root_account_id, -1));
    } else {
        // The branch alone, with the stated root as its depth zero.
        const int base = depth.at(request.root_account_id);
        std::unordered_set<std::string> seen{request.root_account_id};
        std::queue<std::string> branch;
        branch.push(request.root_account_id);
        while (!branch.empty()) {
            const auto id = branch.front();
            branch.pop();
            response.nodes.push_back(node_for(id, depth.at(id) - base));
            const auto kids = children.find(id);
            if (kids == children.end()) {
                continue;
            }
            for (const auto& child : kids->second) {
                if (seen.insert(child).second) {
                    branch.push(child);
                }
            }
        }
    }

    // Shallowest first, so a reader sees the shape in the order it is drawn,
    // and the unrooted rows last so they cannot read as roots.
    std::sort(response.nodes.begin(), response.nodes.end(), [](const auto& a, const auto& b) {
        const auto rank = [](const messaging::reporting_tree_node& n) {
            return n.depth < 0 ? std::numeric_limits<int>::max() : n.depth;
        };
        if (rank(a) != rank(b)) {
            return rank(a) < rank(b);
        }
        return a.username < b.username;
    });

    response.result.outcome = ores::utility::domain::outcome::ok;
    return response;
}

}
