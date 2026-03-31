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
#ifndef ORES_REFDATA_CORE_MESSAGING_PARTY_ID_SCHEME_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_PARTY_ID_SCHEME_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.refdata.api/messaging/party_id_scheme_protocol.hpp"
#include "ores.refdata.core/service/party_id_scheme_service.hpp"

namespace ores::refdata::messaging {

namespace {
inline auto& party_id_scheme_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.refdata.messaging.party_id_scheme_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

class party_id_scheme_handler {
public:
    party_id_scheme_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(party_id_scheme_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::party_id_scheme_service svc(ctx);
        get_party_id_schemes_response resp;
        try {
            resp.party_id_schemes = svc.list_schemes();
            resp.total_available_count =
                static_cast<int>(resp.party_id_schemes.size());
            BOOST_LOG_SEV(party_id_scheme_handler_lg(), debug)
                << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_id_scheme_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(party_id_scheme_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "refdata::party_id_schemes:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::party_id_scheme_service svc(ctx);
        auto req = decode<save_party_id_scheme_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_id_scheme_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            svc.save_scheme(req->data);
            BOOST_LOG_SEV(party_id_scheme_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                save_party_id_scheme_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_id_scheme_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_party_id_scheme_response{
                .success = false, .message = e.what()});
        }
    }

    void del(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(party_id_scheme_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "refdata::party_id_schemes:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::party_id_scheme_service svc(ctx);
        auto req = decode<delete_party_id_scheme_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_id_scheme_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            svc.remove_scheme(req->scheme);
            BOOST_LOG_SEV(party_id_scheme_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                delete_party_id_scheme_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_id_scheme_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_party_id_scheme_response{
                .success = false, .message = e.what()});
        }
    }

    void history(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(party_id_scheme_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::party_id_scheme_service svc(ctx);
        auto req = decode<get_party_id_scheme_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_id_scheme_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto h = svc.get_scheme_history(req->scheme);
            BOOST_LOG_SEV(party_id_scheme_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, get_party_id_scheme_history_response{
                .success = true, .history = std::move(h)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_id_scheme_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_party_id_scheme_history_response{
                .success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::refdata::messaging
#endif
