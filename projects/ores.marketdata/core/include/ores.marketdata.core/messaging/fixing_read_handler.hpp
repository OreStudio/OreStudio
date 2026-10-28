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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_FIXING_READ_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_FIXING_READ_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/fixing_read_operations_protocol.hpp"
#include "ores.marketdata.core/service/fixing_reader.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>
#include <string>

namespace ores::marketdata::messaging {

namespace {
inline auto& fixing_read_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.fixing_read_handler");
    return instance;
}
}

/**
 * @brief NATS message handler for the reads of the durable fixings.
 *
 * The handler owns the session, the permission and the wire. The reads themselves are
 * service::fixing_reader, which a test drives without NATS.
 */
class fixing_read_handler {
public:
    fixing_read_handler(ores::nats::service::client& nats,
                        ores::database::context ctx,
                        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void get_fixing(ores::nats::message msg) {
        handle<get_fixing_request, get_fixing_response>(
            std::move(msg), [](const auto& ctx, const auto& req) {
                return service::fixing_reader::latest(ctx, req);
            });
    }

    void get_fixing_window(ores::nats::message msg) {
        handle<get_fixing_window_request, get_fixing_window_response>(
            std::move(msg), [](const auto& ctx, const auto& req) {
                return service::fixing_reader::window(ctx, req);
            });
    }

private:
    template <typename Request, typename Response, typename Read>
    void handle(ores::nats::message msg, Read read) {
        using namespace ores::logging;
        using ores::service::messaging::error_reply;
        auto req_ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx) {
            error_reply(nats_, msg, req_ctx.error());
            return;
        }
        if (!ores::service::messaging::has_permission(*req_ctx,
                                                      "marketdata::market_fixings:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = ores::service::messaging::decode<Request>(msg);
        if (!req) {
            BOOST_LOG_SEV(fixing_read_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        Response resp;
        try {
            resp = read(*req_ctx, *req);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(fixing_read_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp = {};
            resp.result.outcome = ores::utility::domain::outcome::failed;
            resp.result.code = "internal_error";
            resp.result.message = e.what();
        }
        ores::service::messaging::reply(nats_, msg, resp);
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
