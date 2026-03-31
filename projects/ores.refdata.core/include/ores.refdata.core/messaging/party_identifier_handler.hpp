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
#ifndef ORES_REFDATA_CORE_MESSAGING_PARTY_IDENTIFIER_HANDLER_HPP
#define ORES_REFDATA_CORE_MESSAGING_PARTY_IDENTIFIER_HANDLER_HPP

#include <optional>
#include <boost/uuid/string_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.refdata.api/messaging/party_identifier_protocol.hpp"
#include "ores.refdata.core/service/party_identifier_service.hpp"

namespace ores::refdata::messaging {

namespace {
inline auto& party_identifier_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.refdata.messaging.party_identifier_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

class party_identifier_handler {
public:
    party_identifier_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(party_identifier_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::party_identifier_service svc(ctx);
        auto req = decode<get_party_identifiers_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_identifier_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            boost::uuids::string_generator gen;
            auto items = svc.list_party_identifiers_by_party(
                gen(req->party_id));
            get_party_identifiers_response resp;
            resp.identifiers = std::move(items);
            BOOST_LOG_SEV(party_identifier_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, resp);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_identifier_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_party_identifiers_response{});
        }
    }

    void save(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(party_identifier_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "refdata::party_identifiers:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::party_identifier_service svc(ctx);
        auto req = decode<save_party_identifier_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_identifier_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            svc.save_party_identifier(req->data);
            BOOST_LOG_SEV(party_identifier_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                save_party_identifier_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_identifier_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_party_identifier_response{
                .success = false, .message = e.what()});
        }
    }

    void del(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(party_identifier_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "refdata::party_identifiers:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::party_identifier_service svc(ctx);
        auto req = decode<delete_party_identifier_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(party_identifier_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            boost::uuids::string_generator gen;
            for (const auto& id_str : req->ids)
                svc.remove_party_identifier(gen(id_str));
            BOOST_LOG_SEV(party_identifier_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                delete_party_identifier_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(party_identifier_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_party_identifier_response{
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
