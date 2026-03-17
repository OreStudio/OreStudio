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
#include "ores.iam/messaging/bootstrap_protocol.hpp"
#include "ores.iam/domain/role.hpp"
#include "ores.iam/domain/account_party.hpp"
#include "ores.iam/service/account_service.hpp"
#include "ores.iam/service/account_party_service.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.iam/service/bootstrap_mode_service.hpp"
#include "ores.refdata/domain/party.hpp"
#include "ores.refdata/repository/party_repository.hpp"
#include "ores.iam/repository/tenant_repository.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>

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
        namespace svc_acct = ores::iam::domain::service_accounts;
        auto req = decode<create_initial_admin_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bootstrap_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            service::account_service svc(ctx_);
            auto acct = svc.create_account(
                req->principal, req->email, req->password,
                svc_acct::iam);

            // Associate the admin account with the system party so the
            // JWT gains a party_id and the user can access party-scoped data.
            const auto tenant_id_str = ctx_.tenant_id().to_string();
            refdata::repository::party_repository party_repo(ctx_);
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
                ap.change_commentary = "Bootstrap: associate admin with system party";
                service::account_party_service ap_svc(ctx_);
                ap_svc.save_account_party(ap);
                BOOST_LOG_SEV(bootstrap_handler_lg(), info)
                    << "Associated " << req->principal
                    << " with system party " << boost::uuids::to_string(sys_party.id);
            } else {
                BOOST_LOG_SEV(bootstrap_handler_lg(), warn)
                    << "No system party found for tenant " << tenant_id_str
                    << "; admin account has no party context";
            }

            auto auth_svc =
                std::make_shared<service::authorization_service>(ctx_);
            service::bootstrap_mode_service bms(
                ctx_, ctx_.tenant_id().to_string(), auth_svc);
            bms.exit_bootstrap_mode();
            BOOST_LOG_SEV(bootstrap_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, create_initial_admin_response{
                .success = true,
                .account_id = boost::uuids::to_string(acct.id),
                .tenant_id = acct.tenant_id.to_string()});
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
            // Look up tenant by hostname
            repository::tenant_repository tenant_repo(ctx_);
            auto tenants = tenant_repo.read_latest_by_hostname(req->tenant_hostname);
            if (tenants.empty()) {
                reply(nats_, msg, provision_tenant_response{
                    .success = false,
                    .error_message = "Tenant not found for hostname: " +
                        req->tenant_hostname});
                return;
            }
            const auto& t = tenants.front();
            auto tid_result = ores::utility::uuid::tenant_id::from_uuid(t.id);
            if (!tid_result) {
                reply(nats_, msg, provision_tenant_response{
                    .success = false,
                    .error_message = "Invalid tenant ID: " + tid_result.error()});
                return;
            }

            // Create tenant-scoped context
            auto tenant_ctx = ctx_.with_tenant(*tid_result, req->principal);
            const auto tenant_id_str = tid_result->to_string();

            // Create account in tenant context
            service::account_service svc(tenant_ctx);
            auto acct = svc.create_account(
                req->principal, req->email, req->password,
                svc_acct::iam);

            // Ensure a system party exists for this tenant
            refdata::repository::party_repository party_repo(tenant_ctx);
            auto system_parties = party_repo.read_system_party(tenant_id_str);
            if (system_parties.empty()) {
                refdata::domain::party sys_party;
                sys_party.id = boost::uuids::random_generator()();
                sys_party.tenant_id = tenant_id_str;
                sys_party.full_name = t.name + " System Party";
                sys_party.short_code = "system_party";
                sys_party.party_category = "System";
                sys_party.party_type = "Internal";
                sys_party.business_center_code = "WRLD";
                sys_party.status = "Active";
                sys_party.modified_by = req->principal;
                sys_party.performed_by = req->principal;
                sys_party.change_reason_code = "system.initial_load";
                sys_party.change_commentary =
                    "Provision tenant: create system party";
                party_repo.write(sys_party);
                system_parties = party_repo.read_system_party(tenant_id_str);
                BOOST_LOG_SEV(bootstrap_handler_lg(), info)
                    << "Created system party for tenant " << tenant_id_str;
            }

            // Associate the account with the system party
            if (!system_parties.empty()) {
                const auto& sys_party = system_parties.front();
                domain::account_party ap;
                ap.account_id = acct.id;
                ap.party_id = sys_party.id;
                ap.tenant_id = tenant_id_str;
                ap.modified_by = req->principal;
                ap.performed_by = req->principal;
                ap.change_reason_code = "system.initial_load";
                ap.change_commentary = "Provision tenant: associate admin with system party";
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
