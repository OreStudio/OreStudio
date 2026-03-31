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
#ifndef ORES_IAM_MESSAGING_ACCOUNT_PARTY_HANDLER_HPP
#define ORES_IAM_MESSAGING_ACCOUNT_PARTY_HANDLER_HPP

#include <stdexcept>
#include <boost/uuid/string_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.iam.api/messaging/account_party_protocol.hpp"
#include "ores.iam.core/service/account_party_service.hpp"

namespace ores::iam::messaging {

namespace {

inline auto& account_party_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.iam.messaging.account_party_handler");
    return instance;
}

} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;

class account_party_handler {
public:
    account_party_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        ores::security::jwt::jwt_authenticator signer)
        : nats_(nats), ctx_(std::move(ctx)), signer_(std::move(signer)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(account_party_handler_lg(), msg);
        try {
            service::account_party_service svc(ctx_);
            auto aps = svc.list_account_parties();
            get_account_parties_response resp;
            resp.total_available_count =
                static_cast<int>(aps.size());
            resp.account_parties = std::move(aps);
            BOOST_LOG_SEV(account_party_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_party_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_account_parties_response{});
        }
    }

    void by_account(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(account_party_handler_lg(), msg);
        auto req = decode<get_account_parties_by_account_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(account_party_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            service::account_party_service svc(ctx_);
            boost::uuids::string_generator sg;
            auto aps = svc.list_account_parties_by_account(
                sg(req->account_id));
            get_account_parties_by_account_response resp;
            resp.account_parties = std::move(aps);
            BOOST_LOG_SEV(account_party_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_party_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg,
                get_account_parties_by_account_response{});
        }
    }

    void save(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(account_party_handler_lg(), msg);
        auto req = decode<save_account_party_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(account_party_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto ctx_expected = ores::service::service::make_request_context(
                ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
            if (!ctx_expected) {
                error_reply(nats_, msg, ctx_expected.error());
                return;
            }
            const auto& ctx = *ctx_expected;
            if (!has_permission(ctx, "iam::accounts:update")) {
                error_reply(nats_, msg, ores::service::error_code::forbidden);
                return;
            }
            service::account_party_service svc(ctx);
            for (auto ap : req->account_parties) {
                stamp(ap, ctx);
                svc.save_account_party(ap);
            }
            BOOST_LOG_SEV(account_party_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                save_account_party_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_party_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_account_party_response{
                .success = false, .message = e.what()});
        }
    }

    void del(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(account_party_handler_lg(), msg);
        auto req = decode<delete_account_party_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(account_party_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>{signer_});
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "iam::accounts:update")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        try {
            service::account_party_service svc(ctx);
            boost::uuids::string_generator sg;
            for (const auto& key : req->keys)
                svc.remove_account_party(
                    sg(key.account_id), sg(key.party_id));
            BOOST_LOG_SEV(account_party_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                delete_account_party_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(account_party_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_account_party_response{
                .success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    ores::security::jwt::jwt_authenticator signer_;
};

} // namespace ores::iam::messaging
#endif
