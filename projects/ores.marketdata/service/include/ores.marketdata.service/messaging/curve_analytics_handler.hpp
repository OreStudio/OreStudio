/* -*- mode: c++; tab-width: 4; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_CURVE_ANALYTICS_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_CURVE_ANALYTICS_HANDLER_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.service/app/curve_analytics_reader.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <exception>
#include <optional>
#include <string_view>

namespace ores::marketdata::messaging {

inline auto& curve_analytics_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.curve_analytics_handler");
    return instance;
}

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief The instrument catalogue: the one read both clients search with.
 *
 * It reads market series records, so it asks for the permission those records
 * name, `marketdata::market_series:read`. The party is resolved from the token
 * and never from the request body; the request may only narrow the read to one
 * of the parties the caller already holds.
 */
class curve_analytics_handler {
public:
    curve_analytics_handler(ores::nats::service::client& nats,
                                 ores::database::context ctx,
                                 std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void read(ores::nats::message msg) {
        [[maybe_unused]] const auto correlation_id =
            log_handler_entry(curve_analytics_handler_lg(), msg);
        const auto req_ctx = context_of(msg, "marketdata::market_series:read");
        if (!req_ctx)
            return;
        auto req = decode<get_curve_analytics_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(curve_analytics_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        get_curve_analytics_response resp;
        try {
            resp = service::app::get_curve_analytics(*req_ctx, *req);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(curve_analytics_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp.success = false;
            resp.message = e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    /**
     * @brief The caller's context, or nothing after a refusal has been sent.
     */
    std::optional<ores::database::context> context_of(ores::nats::message& msg,
                                                      std::string_view permission) {
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return std::nullopt;
        }
        if (!permission.empty() && !has_permission(*req_ctx_expected, permission)) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return std::nullopt;
        }
        return std::optional<ores::database::context>{*req_ctx_expected};
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
