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
#ifndef ORES_MARKETDATA_CORE_MESSAGING_SERIES_TICKS_HANDLER_HPP
#define ORES_MARKETDATA_CORE_MESSAGING_SERIES_TICKS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/service/series_tick_reader.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <optional>

namespace ores::marketdata::messaging {

namespace {
inline auto& series_ticks_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.series_ticks_handler");
    return instance;
}
}

using ores::service::messaging::reply;
using ores::service::messaging::decode;
using ores::service::messaging::error_reply;
using namespace ores::logging;

/**
 * @brief NATS message handler for the history call: every tick of one series in a span of
 * time, in one reply.
 *
 * The handler owns the session, the permission and the wire. The read itself is
 * service::series_tick_reader, which a test drives without NATS.
 */
class series_ticks_handler {
public:
    series_ticks_handler(ores::nats::service::client& nats,
                         ores::database::context ctx,
                         std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void get_ticks(ores::nats::message msg) {
        BOOST_LOG_SEV(series_ticks_handler_lg(), debug) << "Handling " << msg.subject;
        auto req_ctx_expected = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx_expected) {
            error_reply(nats_, msg, req_ctx_expected.error());
            return;
        }
        const auto& req_ctx = *req_ctx_expected;
        // The ticks of a series are its observations as they arrive, so the history takes the
        // read permission of the observations.
        if (!ores::service::messaging::has_permission(req_ctx,
                                                      "marketdata::market_observations:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        if (auto req = decode<get_series_ticks_request>(msg)) {
            get_series_ticks_response resp;
            try {
                resp = service::series_tick_reader::read(req_ctx, *req);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(series_ticks_handler_lg(), error)
                    << msg.subject << " failed: " << e.what();
                resp = {};
                resp.result.outcome = ores::utility::domain::outcome::failed;
                resp.result.code = "internal_error";
                resp.result.message = e.what();
            }
            BOOST_LOG_SEV(series_ticks_handler_lg(), debug)
                << "Completed " << msg.subject << " tenant=" << req_ctx.tenant_id().to_string()
                << " visible_parties=" << req_ctx.visible_party_ids().size()
                << " identity=" << req->identity.asset << '/' << req->identity.scope << '/'
                << req->identity.instrument_type << '/' << req->identity.quote_type
                << " fields=" << req->identity.fields.size() << " party=" << req->identity.party_id
                << " outcome=" << (resp.result.code.empty() ? "ok" : resp.result.code)
                << " ticks=" << resp.values.size();
            reply(nats_, msg, resp);
            return;
        }
        BOOST_LOG_SEV(series_ticks_handler_lg(), warn) << "Failed to decode: " << msg.subject;
        error_reply(nats_, msg, ores::service::error_code::bad_request);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
