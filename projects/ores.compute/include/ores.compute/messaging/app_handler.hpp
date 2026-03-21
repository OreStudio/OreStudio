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
#ifndef ORES_COMPUTE_MESSAGING_APP_HANDLER_HPP
#define ORES_COMPUTE_MESSAGING_APP_HANDLER_HPP

#include <optional>
#include <stdexcept>
#include <rfl/json.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.compute/messaging/app_protocol.hpp"
#include "ores.compute/service/app_service.hpp"

namespace ores::compute::messaging {

namespace {
inline auto& app_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.compute.messaging.app_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::error_reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using namespace ores::logging;

class app_handler {
public:
    app_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(app_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::app_service svc(ctx);
        list_apps_response resp;
        try {
            if (auto req = decode<list_apps_request>(msg)) {
                resp.apps = svc.list();
                resp.total_available_count =
                    static_cast<int>(resp.apps.size());
            }
        } catch (...) {}
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(app_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(app_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (auto req = decode<save_app_request>(msg)) {
            try {
                service::app_service svc(ctx);
                stamp(req->app, ctx);
                svc.save(req->app);
                reply(nats_, msg, save_app_response{.success = true});
            } catch (const std::exception& e) {
                reply(nats_, msg, save_app_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(app_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
        BOOST_LOG_SEV(app_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(app_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        get_app_history_response resp;
        try {
            if (auto req = decode<get_app_history_request>(msg)) {
                service::app_service svc(ctx);
                resp.versions = svc.history(req->id);
            }
        } catch (const std::exception& e) {
            resp.success = false;
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
        BOOST_LOG_SEV(app_handler_lg(), debug)
            << "Completed " << msg.subject;
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::compute::messaging

#endif
