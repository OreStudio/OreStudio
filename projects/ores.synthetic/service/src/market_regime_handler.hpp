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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_SYNTHETIC_SERVICE_MARKET_REGIME_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_MARKET_REGIME_HANDLER_HPP

#include "feed_kind_registry.hpp"
#include "ores.analytics.quant/domain/market_regime.hpp"
#include "ores.synthetic.api/messaging/market_regime_protocol.hpp"
#include <boost/log/trivial.hpp>
#include <optional>
#include <string>
#include <utility>

namespace ores::synthetic::service {

namespace {
inline auto& market_regime_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.market_regime_handler");
    return instance;
}
} // namespace

/**
 * @brief Reads and pins the market-wide arrival factor.
 *
 * The factor belongs to the run, not to a row, so it is set here rather than in
 * a generation config: a user pins it once and every feed follows. Pinning is
 * the operational switch a demonstration or a stress run uses, which is why it
 * takes effect at once instead of waiting for the chain to turn.
 *
 * A read needs a session, so it is attributable. A pin needs what starting a
 * feed needs, because it changes what every feed publishes.
 */
class market_regime_handler {
public:
    market_regime_handler(ores::nats::service::client& nats,
                          ores::analytics::quant::domain::market_regime& regime,
                          ores::database::context ctx,
                          std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , regime_(regime)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void get(ores::nats::message msg) {
        using ores::service::messaging::error_reply;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            BOOST_LOG_SEV(market_regime_handler_lg(), warn)
                << "Rejecting get_market_regime request: auth failed.";
            error_reply(nats_, msg, ctx.error());
            return;
        }

        messaging::get_market_regime_response response;
        response.success = true;
        fill(response);
        reply(nats_, msg, response);
    }

    void pin(ores::nats::message msg) {
        using ores::service::messaging::error_reply;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            BOOST_LOG_SEV(market_regime_handler_lg(), warn)
                << "Rejecting pin_market_regime request: auth failed.";
            error_reply(nats_, msg, ctx.error());
            return;
        }
        if (!feed_kind_registry::permits_run(*ctx)) {
            BOOST_LOG_SEV(market_regime_handler_lg(), warn)
                << "Rejecting pin_market_regime request: the caller may not run feeds.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        const auto decoded =
            ores::nats::default_wire_codec().decode<messaging::pin_market_regime_request>(msg.data);
        if (!decoded) {
            BOOST_LOG_SEV(market_regime_handler_lg(), warn)
                << "Rejecting pin_market_regime request: the payload did not decode.";
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        const auto& mode = decoded->mode;
        if (mode == "auto") {
            regime_.unpin();
        } else if (mode == "calm") {
            regime_.pin(ores::analytics::quant::domain::market_regime::state::calm);
        } else if (mode == "active") {
            regime_.pin(ores::analytics::quant::domain::market_regime::state::active);
        } else {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        BOOST_LOG_SEV(market_regime_handler_lg(), info)
            << "The market regime was set to '" << mode << "'.";

        messaging::pin_market_regime_response response;
        response.success = true;
        fill(response);
        reply(nats_, msg, response);
    }

private:
    template <typename Response>
    void fill(Response& response) const {
        const auto state = regime_.current();
        response.state = state == ores::analytics::quant::domain::market_regime::state::active ?
                             "active" :
                             "calm";
        response.multiplier = regime_.multiplier();
        response.pinned = regime_.is_pinned();
    }

    ores::nats::service::client& nats_;
    ores::analytics::quant::domain::market_regime& regime_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
