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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_CURVE_REPUBLISH_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_CURVE_REPUBLISH_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/curve_republish_protocol.hpp"
#include "ores.marketdata.service/app/curve_republish_service.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>

namespace ores::marketdata::messaging {

namespace {
inline auto& curve_republish_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.curve_republish_handler");
    return instance;
}
}

using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief NATS request/reply handler for republish_curve_request -- the on-demand trigger for
 * curve_republish_service::republish(), the same entry point the Qt client, shell/CLI, or a
 * scheduler job would all call.
 */
class curve_republish_handler {
public:
    curve_republish_handler(ores::nats::service::client& nats,
                            ores::database::context ctx,
                            std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void republish(ores::nats::message msg) {
        BOOST_LOG_SEV(curve_republish_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;

        republish_curve_response resp;
        if (auto req = decode<republish_curve_request>(msg)) {
            try {
                const auto bootstrap_config_id =
                    boost::lexical_cast<boost::uuids::uuid>(req->bootstrap_config_id);
                ores::marketdata::service::app::curve_republish_service::republish(
                    req_ctx, bootstrap_config_id, req->as_of);
                resp.success = true;
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(curve_republish_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp.success = false;
                resp.message = e.what();
            }
        } else {
            BOOST_LOG_SEV(curve_republish_handler_lg(), warn)
                << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        BOOST_LOG_SEV(curve_republish_handler_lg(), debug) << "Completed " << msg.subject;
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
