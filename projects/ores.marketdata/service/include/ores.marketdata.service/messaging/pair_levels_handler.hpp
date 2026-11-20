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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_PAIR_LEVELS_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_PAIR_LEVELS_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/messaging/fixing_read_operations_protocol.hpp"
#include "ores.marketdata.core/service/fixing_reader.hpp"
#include "ores.marketdata.service/app/fixing_sources.hpp"
#include "ores.marketdata.service/app/pair_levels.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <exception>
#include <optional>
#include <string>

namespace ores::marketdata::messaging {

namespace {
inline auto& pair_levels_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.pair_levels_handler");
    return instance;
}
}

/**
 * @brief NATS message handler for a pair's daily levels, composed from a party's fixings.
 *
 * The handler owns the session, the permission and the wire. It reads the party's fixing
 * sources and lends the composition the fixing window read, so the pair is read under the
 * same entitlement as each of its fixings.
 */
class pair_levels_handler {
public:
    pair_levels_handler(ores::nats::service::client& nats,
                        const ores::database::context& ctx,
                        std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(verifier) {}

    void get_pair_levels(ores::nats::message msg) {
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
        auto req = ores::service::messaging::decode<get_pair_levels_request>(msg);
        if (!req) {
            ORES_LOG_SEV(pair_levels_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        get_pair_levels_response resp;
        try {
            resp = read(*req_ctx, *req);
        } catch (const std::exception& e) {
            ORES_LOG_SEV(pair_levels_handler_lg(), error) << msg.subject << " failed: " << e.what();
            resp = {};
            resp.result.outcome = ores::utility::domain::outcome::failed;
            resp.result.code = "internal_error";
            resp.result.message = e.what();
        }
        ores::service::messaging::reply(nats_, msg, resp);
    }

private:
    static get_pair_levels_response read(const ores::database::context& ctx,
                                         const get_pair_levels_request& req) {
        const auto sources = service::app::read_fixing_sources(ctx);
        if (!sources) {
            get_pair_levels_response resp;
            resp.result.outcome = ores::utility::domain::outcome::failed;
            resp.result.code = "fixing_sources_unread";
            resp.result.message = sources.error();
            return resp;
        }
        const auto party = sources->find({ctx.tenant_id().to_string(), req.party_id});
        if (party == sources->end()) {
            get_pair_levels_response resp;
            resp.result.outcome = ores::utility::domain::outcome::missing;
            resp.result.code = "pair_unserved";
            resp.result.message = "The party " + req.party_id + " holds no fixing series, so " +
                                  req.base_currency_code + "/" + req.quote_currency_code +
                                  " cannot be composed.";
            return resp;
        }
        return service::app::compose_pair_levels(
            party->second,
            req,
            [&ctx, &req](const std::string& provider_code, const std::string& item) {
                return service::fixing_reader::window(ctx,
                                                      {.item = item,
                                                       .provider_code = provider_code,
                                                       .party_id = req.party_id,
                                                       .to = req.to,
                                                       .days = req.days});
            });
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
