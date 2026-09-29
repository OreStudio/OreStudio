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

#include "ores.database/domain/context.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.api/messaging/bootstrap_protocol.hpp"
#include "ores.iam.core/messaging/principal.hpp"
#include "ores.iam.core/repository/tenant_lookups.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.iam.core/service/bootstrap_mode_service.hpp"
#include "ores.iam.core/service/cache/party_cache.hpp"
#include "ores.iam.core/service/tenant_provisioning_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/crypto/password_hasher.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.utility/version/version.hpp"
#include <algorithm>
#include <memory>
#include <stdexcept>
#include <string_view>
#include <thread>

namespace ores::iam::messaging {

namespace {

inline auto& bootstrap_handler_lg() {
    static auto instance = ores::logging::make_logger("ores.iam.messaging.bootstrap_handler");
    return instance;
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class bootstrap_handler {
public:
    bootstrap_handler(ores::nats::service::client& nats,
                      ores::database::context ctx,
                      ores::security::jwt::jwt_authenticator signer,
                      std::shared_ptr<service::cache::party_cache> party_cache)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , signer_(std::move(signer))
        , party_cache_(std::move(party_cache)) {}

    void status(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(bootstrap_handler_lg(), msg);
        try {
            auto auth_svc = std::make_shared<service::authorization_service>(ctx_);
            service::bootstrap_mode_service bms(
                ctx_, database::service::tenant_context::system_tenant_id, auth_svc);
            /*
             * A deployment with no tenant of its own has nothing in it, so a
             * screen asks this alongside the administrator question: an
             * installation is brought to life in one sitting, and a browser
             * that closed halfway through it belongs on the same screen when it
             * comes back. The system tenant is the deployment's own bookkeeping
             * and is not a tenant somebody set up.
             */
            const auto tenants = repository::read_all_active_tenants(ctx_);
            const bool has_tenant = std::ranges::any_of(
                tenants, [](const auto& tenant) { return !tenant.tenant_id.is_system(); });
            BOOST_LOG_SEV(bootstrap_handler_lg(), debug) << "Completed " << msg.subject;
            /*
             * The version travels with this read because it is the one a
             * browser makes before it has a session, and a screen states it
             * whether or not the deployment still needs an administrator.
             */
            reply(nats_,
                  msg,
                  bootstrap_status_response{.is_in_bootstrap_mode = bms.is_in_bootstrap_mode(),
                                            .has_tenant = has_tenant,
                                            .version = utility::version::full_version_string()});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(bootstrap_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  bootstrap_status_response{.is_in_bootstrap_mode = false,
                                            .message = e.what(),
                                            .version = utility::version::full_version_string()});
        }
    }

    void create_admin(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(bootstrap_handler_lg(), msg);
        auto req = decode<create_initial_admin_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bootstrap_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }

        // Guard: reject if system is not in bootstrap mode.
        {
            auto auth_svc = std::make_shared<service::authorization_service>(ctx_);
            service::bootstrap_mode_service bms(
                ctx_, database::service::tenant_context::system_tenant_id, auth_svc);
            if (!bms.is_in_bootstrap_mode()) {
                BOOST_LOG_SEV(bootstrap_handler_lg(), warn)
                    << "Rejected " << msg.subject << ": system is not in bootstrap mode";
                reply(nats_,
                      msg,
                      create_initial_admin_response{
                          .success = false, .error_message = "System is not in bootstrap mode"});
                return;
            }
        }

        try {
            using ores::database::repository::execute_parameterized_string_query;

            // Hash the password in C++ — pgcrypto is not required in the DB.
            const auto password_hash = ores::security::crypto::password_hasher::hash(req->password);

            // The stored procedure creates the account, assigns SuperAdmin,
            // associates with the system party, and exits bootstrap mode. Its
            // audit columns take the username, which is the part of the
            // principal before the hostname.
            const auto username = username_of(req->principal);
            const auto results = execute_parameterized_string_query(
                ctx_,
                "SELECT ores_iam_create_initial_admin_fn($1, $2, $3, $4)::text",
                {username, req->email, password_hash, username},
                bootstrap_handler_lg(),
                "Creating initial admin");

            if (results.empty()) {
                reply(nats_,
                      msg,
                      create_initial_admin_response{
                          .success = false, .error_message = "Procedure returned no account_id"});
                return;
            }

            const auto& account_id_str = results[0];

            // Reload the system-tenant party cache: the SQL function created
            // the system party and associated it with the admin account, but
            // no NATS event is published for bootstrap SQL operations.
            std::thread([pc = party_cache_]() {
                (void)pc->load(ores::database::service::tenant_context::system_tenant_id);
            }).detach();

            BOOST_LOG_SEV(bootstrap_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_,
                  msg,
                  create_initial_admin_response{.success = true,
                                                .account_id = account_id_str,
                                                .tenant_id = ctx_.tenant_id().to_string()});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(bootstrap_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(nats_,
                  msg,
                  create_initial_admin_response{.success = false, .error_message = e.what()});
        }
    }

    void provision_tenant(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id = log_handler_entry(bootstrap_handler_lg(), msg);
        auto req = decode<provision_tenant_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(bootstrap_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            // The tenant and its administrator are created the way every
            // provisioning verb creates them, so this verb and the generic one
            // that replaces it (iam.v1.tenants.provision) cannot drift apart
            // while both are reachable.
            const auto created =
                service::tenant_provisioning_service(ctx_).provision(req->type,
                                                                     req->code,
                                                                     req->name,
                                                                     req->hostname,
                                                                     req->description,
                                                                     username_of(req->principal),
                                                                     req->email,
                                                                     req->password);

            // Reload the new tenant's party cache: the SQL provisioner created
            // the system party directly, no NATS event is published for it.
            std::thread([pc = party_cache_, tid = created.tenant_id]() {
                (void)pc->load(tid);
            }).detach();

            BOOST_LOG_SEV(bootstrap_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_,
                  msg,
                  provision_tenant_response{.success = true,
                                            .account_id = created.account_id,
                                            .tenant_id = created.tenant_id});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(bootstrap_handler_lg(), error) << msg.subject << " failed: " << e.what();
            reply(
                nats_, msg, provision_tenant_response{.success = false, .error_message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
    std::shared_ptr<service::cache::party_cache> party_cache_;
};

} // namespace ores::iam::messaging
#endif
