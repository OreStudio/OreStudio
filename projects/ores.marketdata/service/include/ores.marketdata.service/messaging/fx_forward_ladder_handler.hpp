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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_FX_FORWARD_LADDER_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_FX_FORWARD_LADDER_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/fx_forward_operation_protocol.hpp"
#include "ores.marketdata.service/app/fx_forward_ladder_service.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>

namespace ores::marketdata::messaging {

namespace {
inline auto& fx_forward_ladder_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.fx_forward_ladder_handler");
    return instance;
}
}

/// NATS request/reply handler for the FX forward ladder read.
class fx_forward_ladder_handler {
public:
    fx_forward_ladder_handler(ores::nats::service::client& nats,
                              ores::database::context ctx,
                              std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void read(ores::nats::message msg) {
        using namespace ores::logging;
        using namespace ores::service::messaging;
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(fx_forward_ladder_handler_lg(), msg);
        auto req_ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx) {
            error_reply(nats_, msg, req_ctx.error());
            return;
        }
        if (!has_permission(*req_ctx, "marketdata::market_observations:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        const auto req = decode<get_fx_forward_ladder_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(fx_forward_ladder_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        reply(nats_,
              msg,
              ores::marketdata::service::app::fx_forward_ladder_service::read(*req_ctx, *req));
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
