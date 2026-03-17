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
#ifndef ORES_REFDATA_MESSAGING_ROUNDING_TYPE_HANDLER_HPP
#define ORES_REFDATA_MESSAGING_ROUNDING_TYPE_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.refdata/messaging/rounding_type_protocol.hpp"
#include "ores.refdata/service/rounding_type_service.hpp"

namespace ores::refdata::messaging {

namespace {
inline auto& rounding_type_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.refdata.messaging.rounding_type_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using namespace ores::logging;

class rounding_type_handler {
public:
    rounding_type_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(rounding_type_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::rounding_type_service svc(ctx);
        get_rounding_types_response resp;
        try {
            resp.rounding_types = svc.list_types();
            resp.total_available_count =
                static_cast<int>(resp.rounding_types.size());
            BOOST_LOG_SEV(rounding_type_handler_lg(), debug)
                << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(rounding_type_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(rounding_type_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::rounding_type_service svc(ctx);
        auto req = decode<save_rounding_type_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(rounding_type_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            stamp(req->data, ctx);
            svc.save_type(req->data);
            BOOST_LOG_SEV(rounding_type_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                save_rounding_type_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(rounding_type_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_rounding_type_response{
                .success = false, .message = e.what()});
        }
    }

    void del(ores::nats::message msg) {
        BOOST_LOG_SEV(rounding_type_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::rounding_type_service svc(ctx);
        auto req = decode<delete_rounding_type_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(rounding_type_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            svc.remove_type(req->type);
            BOOST_LOG_SEV(rounding_type_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg,
                delete_rounding_type_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(rounding_type_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_rounding_type_response{
                .success = false, .message = e.what()});
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(rounding_type_handler_lg(), debug)
            << "Handling " << msg.subject;
        const auto ctx = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        service::rounding_type_service svc(ctx);
        auto req = decode<get_rounding_type_history_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(rounding_type_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            auto h = svc.get_type_history(req->type);
            BOOST_LOG_SEV(rounding_type_handler_lg(), debug)
                << "Completed " << msg.subject;
            reply(nats_, msg, get_rounding_type_history_response{
                .success = true, .history = std::move(h)});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(rounding_type_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, get_rounding_type_history_response{
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
