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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_FX_FORWARD_PUBLISH_OPERATIONS_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_FX_FORWARD_PUBLISH_OPERATIONS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/messaging/fx_forward_operation_protocol.hpp"
#include "ores.marketdata.service/app/derived_market_object_kind.hpp"
#include "ores.marketdata.service/app/fx_forward_publication_kind.hpp"
#include "ores.marketdata.service/app/publication_kind_codes.hpp"
#include "ores.marketdata.service/app/publication_lifecycle.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>
#include <string>

namespace ores::marketdata::messaging {

inline auto& fx_forward_publish_operations_handler_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.marketdata.messaging.fx_forward_publish_operations_handler");
    return instance;
}

/**
 * @brief The approval-gated publish of a party's FX forward set, and the read of a published one.
 *
 * The maker's permission is the one the kind names, as for an IR curve publish, so the gate moves
 * when the tenant moves the kind. The read needs the permission to read market observations.
 */
class fx_forward_publish_operations_handler {
public:
    fx_forward_publish_operations_handler(
        ores::nats::service::client& nats,
        const ores::database::context& ctx,
        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(verifier) {}

    void publish(ores::nats::message msg) {
        using namespace ores::logging;
        using namespace ores::service::messaging;
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(fx_forward_publish_operations_handler_lg(), msg);
        auto req_ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx) {
            error_reply(nats_, msg, req_ctx.error());
            return;
        }
        std::string maker_permission;
        try {
            maker_permission = ores::marketdata::service::app::raise_permission_of(
                *req_ctx, ores::marketdata::service::app::fx_forward_publish_kind_code);
        } catch (const std::exception& e) {
            ORES_LOG_SEV(fx_forward_publish_operations_handler_lg(), error)
                << msg.subject << " refused: " << e.what();
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        if (!maker_permission.empty() && !has_permission(*req_ctx, maker_permission)) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        const auto req = decode<publish_fx_forward_request>(msg);
        if (!req) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        publish_fx_forward_response resp;
        try {
            answer(*req_ctx, *req, resp);
        } catch (const std::exception& e) {
            resp.success = false;
            resp.message = e.what();
        }
        ORES_LOG_SEV(fx_forward_publish_operations_handler_lg(), info)
            << "FX forward publish of " << req->pair_code << ": "
            << (resp.success ? resp.state_code : "refused: " + resp.message);
        reply(nats_, msg, resp);
    }

    void read(ores::nats::message msg) {
        using namespace ores::logging;
        using namespace ores::service::messaging;
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(fx_forward_publish_operations_handler_lg(), msg);
        auto req_ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx) {
            error_reply(nats_, msg, req_ctx.error());
            return;
        }
        if (!has_permission(*req_ctx, "marketdata::market_observations:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        const auto req = decode<get_published_fx_forward_request>(msg);
        if (!req) {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        reply(nats_,
              msg,
              ores::marketdata::service::app::fx_forward_published_reader::read(*req_ctx, *req));
    }

private:
    /// The raise, or the apply of an approved request, through the one lifecycle.
    static void answer(const ores::database::context& ctx,
                       const publish_fx_forward_request& req,
                       publish_fx_forward_response& resp) {
        using namespace ores::marketdata::service::app;
        const auto party = ores::service::messaging::session_party(ctx, "A forward set");
        const auto answered =
            answer_publication(ctx,
                               derived_kind::fx_forward,
                               req.pair_code,
                               std::chrono::floor<std::chrono::microseconds>(req.as_of),
                               req.approval_request_id,
                               req.reason,
                               party);
        resp.approval_request_id = answered.request_id;
        resp.state_code = answered.state_code;
        resp.success = answered.outcome == publication_outcome::published;
        resp.message = answered.message;
        resp.published_at = answered.published_at;
        resp.published_by = answered.published_by;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
