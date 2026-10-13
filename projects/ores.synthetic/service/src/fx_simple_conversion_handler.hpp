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
#ifndef ORES_SYNTHETIC_SERVICE_FX_SIMPLE_CONVERSION_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_FX_SIMPLE_CONVERSION_HANDLER_HPP

#include "feed_kind_registry.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.security/jwt/jwt_authenticator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.synthetic.api/feeds/fx_simple_conversion.hpp"
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include "ores.synthetic.api/messaging/convert_fx_simple_protocol.hpp"
#include <exception>
#include <optional>
#include <string>

namespace ores::synthetic::service {

namespace {
inline auto& fx_simple_conversion_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.fx_simple_conversion_handler");
    return instance;
}
}

/**
 * @brief Converts an FX feed's mixture between the simple and the advanced view.
 *
 * Stateless: nothing is read or stored. Gated by the FX kind's config
 * permission, as the FX previews are.
 */
class fx_simple_conversion_handler {
public:
    fx_simple_conversion_handler(ores::nats::service::client& nats,
                                 ores::database::context ctx,
                                 std::optional<ores::security::jwt::jwt_authenticator> verifier,
                                 const feed_kind_registry& registry)
        : nats_(nats)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier))
        , registry_(registry) {}

    void convert(ores::nats::message msg) {
        using namespace ores::logging;
        using namespace ores::synthetic::messaging;
        using ores::service::messaging::decode;
        using ores::service::messaging::error_reply;
        using ores::service::messaging::has_permission;
        using ores::service::messaging::reply;

        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            error_reply(nats_, msg, ctx.error());
            return;
        }
        const auto* kind = registry_.find(std::string(ores::synthetic::feed::fx_spot_feed_kind));
        if (kind == nullptr || !has_permission(*ctx, kind->config_permission)) {
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        convert_fx_simple_response resp;
        auto req = decode<convert_fx_simple_request>(msg);
        if (!req) {
            resp.message = "Failed to decode convert request.";
            reply(nats_, msg, resp);
            return;
        }

        try {
            namespace feed = ores::synthetic::feed;
            if (req->to_simple) {
                const auto reading = feed::fx_simple_from_mixture({.means = req->gmm_means,
                                                                   .stdevs = req->gmm_stdevs,
                                                                   .weights = req->gmm_weights},
                                                                  req->process_type,
                                                                  req->initial_price,
                                                                  req->ticks_per_hour);
                resp.has_simple_form = reading.exact;
                resp.message = reading.reason;
                resp.trend_pct_per_hour = reading.view.trend_pct_per_hour;
                resp.volatility_pct_per_hour = reading.view.volatility_pct_per_hour;
                resp.jump_share_pct = reading.view.jump_share_pct;
                resp.half_life_minutes = reading.view.half_life_minutes;
            } else {
                const auto out = feed::fx_mixture_from_simple(
                    {.trend_pct_per_hour = req->trend_pct_per_hour,
                     .volatility_pct_per_hour = req->volatility_pct_per_hour,
                     .jump_share_pct = req->jump_share_pct,
                     .half_life_minutes = req->half_life_minutes},
                    req->process_type,
                    req->initial_price,
                    req->ticks_per_hour);
                resp.has_simple_form = true;
                resp.gmm_means = out.means;
                resp.gmm_stdevs = out.stdevs;
                resp.gmm_weights = out.weights;
            }
            resp.success = true;
        } catch (const std::exception& e) {
            resp.message = e.what();
            BOOST_LOG_SEV(fx_simple_conversion_handler_lg(), warn)
                << "convert_fx_simple refused: " << e.what();
        }
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
    const feed_kind_registry& registry_;
};

}

#endif
