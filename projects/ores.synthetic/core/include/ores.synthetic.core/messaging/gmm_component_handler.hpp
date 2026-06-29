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
#ifndef ORES_SYNTHETIC_CORE_MESSAGING_GMM_COMPONENT_HANDLER_HPP
#define ORES_SYNTHETIC_CORE_MESSAGING_GMM_COMPONENT_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/messaging/gmm_component_protocol.hpp"
#include "ores.synthetic.core/service/gmm_component_service.hpp"
#include <optional>

namespace ores::synthetic::messaging {

namespace {
inline auto& gmm_component_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.messaging.gmm_component_handler");
    return instance;
}
} // namespace

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using namespace ores::logging;

class gmm_component_handler {
public:
    gmm_component_handler(ores::nats::service::client& nats,
                          ores::database::context ctx,
                          std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void list(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(gmm_component_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        service::gmm_component_service svc(ctx);
        get_gmm_components_response resp;
        auto req = decode<get_gmm_components_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(gmm_component_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            reply(nats_, msg, resp);
            return;
        }
        try {
            resp.components = svc.list_components(static_cast<std::uint32_t>(req->offset),
                                                  static_cast<std::uint32_t>(req->limit));
            resp.total_available_count = static_cast<int>(svc.count_components());
            BOOST_LOG_SEV(gmm_component_handler_lg(), debug) << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(gmm_component_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
        }
        reply(nats_, msg, resp);
    }

    void save(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(gmm_component_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "synthetic::gmm_components:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::gmm_component_service svc(ctx);
        auto req = decode<save_gmm_component_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(gmm_component_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            svc.save_component(req->data);
            BOOST_LOG_SEV(gmm_component_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, save_gmm_component_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(gmm_component_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, save_gmm_component_response{.success = false, .message = e.what()});
        }
    }

    void remove(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(gmm_component_handler_lg(), msg);
        auto ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx_expected) {
            error_reply(nats_, msg, ctx_expected.error());
            return;
        }
        const auto& ctx = *ctx_expected;
        if (!has_permission(ctx, "synthetic::gmm_components:delete")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        service::gmm_component_service svc(ctx);
        auto req = decode<delete_gmm_component_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(gmm_component_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            return;
        }
        try {
            svc.delete_components(req->ids);
            BOOST_LOG_SEV(gmm_component_handler_lg(), debug) << "Completed " << msg.subject;
            reply(nats_, msg, delete_gmm_component_response{.success = true});
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(gmm_component_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            reply(nats_, msg, delete_gmm_component_response{.success = false, .message = e.what()});
        }
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

} // namespace ores::synthetic::messaging
#endif
