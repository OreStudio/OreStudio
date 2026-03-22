/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_IAM_MESSAGING_BOOTSTRAP_HANDLER_HPP
#define ORES_IAM_MESSAGING_BOOTSTRAP_HANDLER_HPP

#include <memory>
#include <stdexcept>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.iam.api/messaging/bootstrap_protocol.hpp"
#include "ores.iam.api/domain/role.hpp"
#include "ores.iam.api/domain/account_party.hpp"
#include "ores.iam.core/service/account_service.hpp"
#include "ores.iam.core/service/account_party_service.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.iam.core/service/bootstrap_mode_service.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.security/crypto/password_hasher.hpp"
#include "ores.refdata/domain/party.hpp"
#include "ores.refdata/repository/party_repository.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_io.hpp>

namespace ores::iam::messaging {

namespace {

inline auto& bootstrap_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.iam.messaging.bootstrap_handler");
    return instance;
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;

class bootstrap_handler {
public:
    bootstrap_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::security::jwt::jwt_authenticator signer)
        : nats_(nats), ctx_(std::move(ctx)), signer_(std::move(signer)) {}

    void status(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(bootstrap_handler_lg(), debug)
            << "Handling " << msg.subject;
        try {
            auto auth_svc =
                std::make_shared<service::authorization_service>(ctx_);
            service::bootstrap_mode_service bms(
                ctx_, ctx_.tenant_id().to_string(), auth_svc);
            BOOST_LOG_SEV(bootstrap_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, bootstrap_status_response{
                .is_in_bootstrap_mode = bms.is_in_bootstrap_mode()});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(bootstrap_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, bootstrap_status_response{
                .is_in_bootstrap_mode = false,
                .message = e.what()});
        }
    }

    void create_admin(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(bootstrap_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<create_initial_admin_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bootstrap_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }

        // Guard: reject if system is not in bootstrap mode.
        {
            auto auth_svc =
                std::make_shared<service::authorization_service>(ctx_);
            service::bootstrap_mode_service bms(
                ctx_, ctx_.tenant_id().to_string(), auth_svc);
            if (!bms.is_in_bootstrap_mode()) {
                BOOST_LOG_SEV(bootstrap_handler_lg(), warn)
                    << "Rejected " << msg.subject
                    << ": system is not in bootstrap mode";
                reply(nats_, msg, create_initial_admin_response{
                    .success = false,
                    .error_message = "System is not in bootstrap mode"});
                return;
            }
        }

        try {
            using ores::database::repository::execute_parameterized_string_query;

            // Hash the password in C++ — pgcrypto is not required in the DB.
            const auto password_hash =
                ores::security::crypto::password_hasher::hash(req->password);

            // The stored procedure creates the account, assigns SuperAdmin,
            // associates with the system party, and exits bootstrap mode.
            const auto results = execute_parameterized_string_query(ctx_,
                "SELECT ores_iam_create_initial_admin_fn($1, $2, $3, $4)::text",
                {req->principal, req->email, password_hash, req->principal},
                bootstrap_handler_lg(), "Creating initial admin");

            if (results.empty()) {
                reply(nats_, msg, create_initial_admin_response{
                    .success = false,
                    .error_message = "Procedure returned no account_id"});
                return;
            }

            const auto& account_id_str = results[0];
            BOOST_LOG_SEV(bootstrap_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, create_initial_admin_response{
                .success = true,
                .account_id = account_id_str,
                .tenant_id = ctx_.tenant_id().to_string()});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(bootstrap_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, create_initial_admin_response{
                .success = false, .error_message = e.what()});
        }
    }

    void provision_tenant(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(bootstrap_handler_lg(), debug)
            << "Handling " << msg.subject;
        namespace svc_acct = ores::iam::domain::service_accounts;
        auto req = decode<provision_tenant_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bootstrap_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            using ores::database::service::tenant_context;
            using ores::database::repository::execute_parameterized_string_query;

            // Call the SQL provisioner in system tenant context. The procedure
            // creates the tenant, copies all system-tenant reference data (roles,
            // permissions, lookup tables), seeds the WRLD business centre, creates
            // the system party, and sets the bootstrap mode flag.
            auto sys_ctx = tenant_context::with_system_tenant(ctx_);
            // The actor for the provisioning operation is the IAM service
            // account, not the new tenant admin (req->principal). The admin
            // username is not yet a known account at this point and would
            // fail the modified_by validation in the SQL trigger.
            const auto results = execute_parameterized_string_query(sys_ctx,
                "SELECT ores_iam_provision_tenant_fn($1, $2, $3, $4, $5, $6)::text",
                {req->type, req->code, req->name, req->hostname,
                 req->description, std::string(svc_acct::iam)},
                bootstrap_handler_lg(), "Provisioning tenant");

            if (results.empty()) {
                reply(nats_, msg, provision_tenant_response{
                    .success = false,
                    .error_message = "Provisioner returned no tenant_id"});
                return;
            }

            const auto& tenant_id_str = results[0];
            BOOST_LOG_SEV(bootstrap_handler_lg(), info)
                << "Provisioned tenant: " << req->code
                << " (id: " << tenant_id_str << ")";

            // Create the admin account in the new tenant's context.
            auto tenant_ctx = tenant_context::with_tenant(ctx_, tenant_id_str);
            service::account_service svc(tenant_ctx);
            auto acct = svc.create_account(
                req->principal, req->email, req->password,
                svc_acct::iam);

            // Associate the admin account with the system party.
            refdata::repository::party_repository party_repo(tenant_ctx);
            auto system_parties = party_repo.read_system_party(tenant_id_str);
            if (!system_parties.empty()) {
                const auto& sys_party = system_parties.front();
                domain::account_party ap;
                ap.account_id = acct.id;
                ap.party_id = sys_party.id;
                ap.tenant_id = tenant_id_str;
                ap.modified_by = req->principal;
                ap.performed_by = req->principal;
                ap.change_reason_code = "system.initial_load";
                ap.change_commentary =
                    "Provision tenant: associate admin with system party";
                service::account_party_service ap_svc(tenant_ctx);
                ap_svc.save_account_party(ap);
                BOOST_LOG_SEV(bootstrap_handler_lg(), info)
                    << "Associated " << req->principal
                    << " with system party "
                    << boost::uuids::to_string(sys_party.id);
            } else {
                BOOST_LOG_SEV(bootstrap_handler_lg(), warn)
                    << "No system party found for tenant " << tenant_id_str
                    << "; account has no party context";
            }

            BOOST_LOG_SEV(bootstrap_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, provision_tenant_response{
                .success = true,
                .account_id = boost::uuids::to_string(acct.id),
                .tenant_id = tenant_id_str});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(bootstrap_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, provision_tenant_response{
                .success = false, .error_message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
