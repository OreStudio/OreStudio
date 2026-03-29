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
#ifndef ORES_VARIABILITY_MESSAGING_SYSTEM_SETTING_HANDLER_HPP
#define ORES_VARIABILITY_MESSAGING_SYSTEM_SETTING_HANDLER_HPP

#include <optional>
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.variability.api/messaging/system_settings_protocol.hpp"
#include "ores.variability.core/service/system_settings_service.hpp"

namespace ores::variability::messaging {

namespace {
inline auto& system_setting_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.variability.messaging.system_setting_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::stamp;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

class system_setting_handler {
public:
    system_setting_handler(ores::nats::service::client& nats,
        ores::database::context ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats), ctx_(std::move(ctx)), verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        BOOST_LOG_SEV(system_setting_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::system_settings_service svc(ctx);
        list_settings_response resp;
        try {
            resp.settings = svc.get_all();
        } catch (...) {}
        BOOST_LOG_SEV(system_setting_handler_lg(), debug)
            << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        BOOST_LOG_SEV(system_setting_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "variability::flags:create")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::system_settings_service svc(ctx);
        if (auto req = decode<save_setting_request>(msg)) {
            try {
                stamp(req->data, ctx);
                svc.save(req->data);
                BOOST_LOG_SEV(system_setting_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg,
                    save_setting_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(system_setting_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, save_setting_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(system_setting_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    void remove(ores::nats::message msg) {
        BOOST_LOG_SEV(system_setting_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "variability::flags:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::system_settings_service svc(ctx);
        if (auto req = decode<delete_setting_request>(msg)) {
            try {
                svc.remove(req->name);
                BOOST_LOG_SEV(system_setting_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg,
                    delete_setting_response{.success = true});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(system_setting_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, delete_setting_response{
                    .success = false, .error_message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(system_setting_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

    void history(ores::nats::message msg) {
        BOOST_LOG_SEV(system_setting_handler_lg(), debug)
            << "Handling " << msg.subject;
        auto ctx_expected = ores::service::service::make_request_context(
            ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::system_settings_service svc(ctx);
        if (auto req = decode<get_setting_history_request>(msg)) {
            try {
                auto hist = svc.get_history(req->name);
                BOOST_LOG_SEV(system_setting_handler_lg(), debug)
                    << "Completed " << msg.subject;
                reply(nats_, msg, get_setting_history_response{
                    .success = true,
                    .history = std::move(hist)});
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(system_setting_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                reply(nats_, msg, get_setting_history_response{
                    .success = false, .message = e.what()});
            }
        } else {
            BOOST_LOG_SEV(system_setting_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::variability::messaging

#endif
