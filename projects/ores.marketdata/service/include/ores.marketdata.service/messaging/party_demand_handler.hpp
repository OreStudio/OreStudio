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
#ifndef ORES_MARKETDATA_SERVICE_MESSAGING_PARTY_DEMAND_HANDLER_HPP
#define ORES_MARKETDATA_SERVICE_MESSAGING_PARTY_DEMAND_HANDLER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.marketdata.api/messaging/party_demand_operations_protocol.hpp"
#include "ores.marketdata.service/app/demand_calculator.hpp"
#include "ores.marketdata.service/app/demand_catalogue_reader.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <boost/uuid/string_generator.hpp>
#include <chrono>
#include <exception>
#include <optional>
#include <string>
#include <utility>

namespace ores::marketdata::messaging {

namespace {
inline auto& party_demand_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.marketdata.messaging.party_demand_handler");
    return instance;
}
}

/**
 * @brief NATS message handler for what one party demands of each of its routes.
 *
 * The demand is computed from the catalogue, as the supervisor computes it for the
 * adapters, so it answers before any adapter is ready.
 */
class party_demand_handler {
public:
    party_demand_handler(ores::nats::service::client& nats,
                         const ores::database::context& ctx,
                         std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , ctx_(ctx)
        , verifier_(verifier) {}

    void get_party_demand(ores::nats::message msg) {
        using namespace ores::logging;
        using ores::service::messaging::error_reply;
        auto req_ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!req_ctx) {
            error_reply(nats_, msg, req_ctx.error());
            return;
        }
        if (!ores::service::messaging::has_permission(*req_ctx, "marketdata::routes:read")) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }
        auto req = ores::service::messaging::decode<get_party_demand_request>(msg);
        if (!req) {
            ORES_LOG_SEV(party_demand_handler_lg(), warn) << "Failed to decode: " << msg.subject;
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }
        get_party_demand_response resp;
        try {
            resp = read(*req_ctx, *req);
        } catch (const std::exception& e) {
            ORES_LOG_SEV(party_demand_handler_lg(), error)
                << msg.subject << " failed: " << e.what();
            resp = {};
            resp.result.outcome = ores::utility::domain::outcome::failed;
            resp.result.code = "internal_error";
            resp.result.message = e.what();
        }
        ores::service::messaging::reply(nats_, msg, resp);
    }

private:
    get_party_demand_response read(const ores::database::context& req_ctx,
                                   const get_party_demand_request& req) const {
        const auto party = boost::uuids::string_generator{}(req.party_id);
        auto catalogue = service::app::read_demand_catalogue(ctx_);
        std::erase_if(catalogue.routes, [&](const domain::route& r) {
            return r.tenant_id != req_ctx.tenant_id() || r.party_id != party;
        });
        get_party_demand_response resp;
        for (auto& demand :
             service::app::compute_party_demand(catalogue, std::chrono::system_clock::now())) {
            demanded_route route;
            route.provider_code = std::move(demand.provider_code);
            route.adapter_id = std::move(demand.adapter_id);
            route.entitled = demand.entitled;
            route.refusal = std::move(demand.refusal);
            for (const auto& [uri, ids] : demand.items)
                route.items.push_back(uri);
            resp.routes.push_back(std::move(route));
        }
        return resp;
    }

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
