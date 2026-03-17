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
#include "ores.iam/service/account_service.hpp"
#include "ores.iam/service/authorization_service.hpp"
#include "ores.iam/service/bootstrap_mode_service.hpp"
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

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
