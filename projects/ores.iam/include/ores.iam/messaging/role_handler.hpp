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
#ifndef ORES_IAM_MESSAGING_ROLE_HANDLER_HPP
#define ORES_IAM_MESSAGING_ROLE_HANDLER_HPP

#include <stdexcept>
#include <boost/uuid/string_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.iam/messaging/authorization_protocol.hpp"
#include "ores.iam/service/authorization_service.hpp"

namespace ores::iam::messaging {

namespace {

inline auto& role_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.iam.messaging.role_handler");
    return instance;
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;

class role_handler {
public:
    role_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::security::jwt::jwt_authenticator signer)
        : nats_(nats), ctx_(std::move(ctx)), signer_(std::move(signer)) {}

    void list(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(role_handler_lg(), debug)
            << "Handling " << msg.subject;
        try {
            service::authorization_service svc(ctx_);
            auto roles = svc.list_roles();
            BOOST_LOG_SEV(role_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                list_roles_response{.roles = std::move(roles)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, list_roles_response{});
        }
    }

    void assign(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(role_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<assign_role_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(role_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            const auto ctx = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            service::authorization_service svc(ctx);
            boost::uuids::string_generator sg;
            svc.assign_role(sg(req->account_id),
                sg(req->role_id), ctx.actor());
            BOOST_LOG_SEV(role_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                assign_role_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, assign_role_response{
                .success = false,
                .error_message = e.what()});
        }
    }

    void revoke(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(role_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<revoke_role_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(role_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            service::authorization_service svc(ctx_);
            boost::uuids::string_generator sg;
            svc.revoke_role(sg(req->account_id),
                sg(req->role_id));
            BOOST_LOG_SEV(role_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                revoke_role_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, revoke_role_response{
                .success = false,
                .error_message = e.what()});
        }
    }

    void by_account(ores::nats::message msg) {
        using namespace ores::logging;
        BOOST_LOG_SEV(role_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto req = decode<get_account_roles_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(role_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            service::authorization_service svc(ctx_);
            boost::uuids::string_generator sg;
            auto roles = svc.get_account_roles(
                sg(req->account_id));
            BOOST_LOG_SEV(role_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                get_account_roles_response{
                    .roles = std::move(roles)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(role_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_account_roles_response{});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
