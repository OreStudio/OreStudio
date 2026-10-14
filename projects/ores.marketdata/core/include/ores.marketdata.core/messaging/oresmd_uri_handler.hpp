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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_ORESMD_URI_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_ORESMD_URI_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/service/oresmd_uri_service.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <optional>
#include <utility>

namespace ores::marketdata::messaging {

namespace {

auto& oresmd_uri_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.oresmd_uri_handler");
    return instance;
}

}

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::has_permission;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief NATS message handler for reading and writing an oresmd URI.
 *
 * Both calls are the codec's grammar and touch no data, so they need the same
 * permission as the series read and nothing more. A URI the codec refuses is
 * an answer with the reason in it. Only a request that cannot be decoded is an
 * error.
 */
class oresmd_uri_handler {
public:
    oresmd_uri_handler(ores::nats::service::client& nats,
                       ores::database::context ctx,
                       std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void parse(ores::nats::message msg) {
        answer<parse_oresmd_uri_request>(std::move(msg), [](const auto& req) {
            return service::oresmd_uri_service::parse(req);
        });
    }

    void compose(ores::nats::message msg) {
        answer<compose_oresmd_uri_request>(std::move(msg), [](const auto& req) {
            return service::oresmd_uri_service::compose(req);
        });
    }

private:
    template <typename Request, typename Work>
    void answer(ores::nats::message msg, Work work) {
        BOOST_LOG_SEV(oresmd_uri_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        if (!has_permission(*req_ctx_expected, "marketdata::market_series:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = decode<Request>(msg);
        if (!req) {
            BOOST_LOG_SEV(oresmd_uri_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        // The work reads the market series store, so it can throw. An exception
        // leaving this handler reaches the transport, which contains it but
        // sends no reply, and the caller then waits out its whole deadline for
        // an answer this handler could have given.
        using response_type = typename Request::response_type;
        try {
            reply(nats_, msg, work(*req));
            BOOST_LOG_SEV(oresmd_uri_handler_lg(), debug) << "Completed " << msg.subject;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(oresmd_uri_handler_lg(), error) << msg.subject << " failed: " << e.what();
            response_type failure;
            failure.success = false;
            failure.message = e.what();
            reply(nats_, msg, failure);
        }
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
