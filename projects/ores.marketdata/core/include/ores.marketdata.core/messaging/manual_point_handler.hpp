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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_MANUAL_POINT_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_MANUAL_POINT_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/service/manual_point_service.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>
#include <stdexcept>

namespace ores::marketdata::messaging {

namespace {

auto& manual_point_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.manual_point_handler");
    return instance;
}

}

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using namespace ores::logging;

/**
 * @brief NATS message handler for keying and clearing a manual point.
 *
 * Both operations write observations, so both need the observation write
 * permission. The write itself is service::manual_point_service, which checks the
 * node against the shape the object declares.
 */
class manual_point_handler {
public:
    manual_point_handler(ores::nats::service::client& nats,
                         ores::database::context ctx,
                         std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void key(ores::nats::message msg) {
        handle<key_manual_point_request, key_manual_point_response>(
            std::move(msg), [](const auto& ctx, const auto& req) {
                return service::manual_point_service::key(ctx, req);
            });
    }

    void clear(ores::nats::message msg) {
        handle<clear_manual_point_request, clear_manual_point_response>(
            std::move(msg), [](const auto& ctx, const auto& req) {
                return service::manual_point_service::clear(ctx, req);
            });
    }

private:
    template <typename Request, typename Response, typename Operation>
    void handle(ores::nats::message msg, Operation operation) {
        BOOST_LOG_SEV(manual_point_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        if (!has_permission(req_ctx, "marketdata::market_observations:write")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        auto req = decode<Request>(msg);
        if (!req) {
            BOOST_LOG_SEV(manual_point_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        Response resp;
        try {
            resp = operation(req_ctx, *req);
        } catch (const std::invalid_argument& e) {
            // The identity the caller stated is not one the codec can spell, so it
            // is the request that is wrong rather than the write.
            BOOST_LOG_SEV(manual_point_handler_lg(), warn)
                << msg.subject << " rejected: " << e.what();
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(manual_point_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            // The exception can name tables or constraints, so it is logged and the
            // caller is told only that the write failed.
            resp.success = false;
            resp.message = "The write failed.";
        }
        BOOST_LOG_SEV(manual_point_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
